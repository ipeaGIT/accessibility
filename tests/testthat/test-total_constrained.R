# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(
    travel_matrix = smaller_matrix,
    land_use_data = get("land_use_data", envir = parent.frame()),
    opportunity = "jobs",
    travel_cost = "travel_time",
    decay_function = decay_exponential(0.1),
    group_by = "mode",
    fill_missing_ids = TRUE,
    detailed_results = FALSE
) {
  total_constrained(
    travel_matrix,
    land_use_data,
    opportunity,
    travel_cost,
    decay_function,
    group_by,
    fill_missing_ids,
    detailed_results
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(decay_function = "a"))
  expect_error(tester(decay_function = mean))
  expect_error(tester(decay_function = get))

  expect_error(tester(opportunity = 1))
  expect_error(tester(opportunity = c("schools", "jobs")))

  expect_error(tester(travel_cost = 1))
  expect_error(tester(travel_cost = c("travel_time", "monetary_cost")))

  expect_error(tester(group_by = 1))
  expect_error(tester(group_by = NA))
  expect_error(tester(group_by = "from_id"))
  expect_error(tester(group_by = c("mode", "mode")))

  expect_error(tester(fill_missing_ids = 1))
  expect_error(tester(fill_missing_ids = c(TRUE, TRUE)))
  expect_error(tester(fill_missing_ids = NA))

  expect_error(tester(detailed_results = 1))
  expect_error(tester(detailed_results = c(TRUE, TRUE)))
  expect_error(tester(detailed_results = NA))

  expect_error(tester(as.list(travel_matrix)))
  expect_error(tester(travel_matrix[, .(oi = from_id, to_id, travel_time)]))
  expect_error(tester(travel_matrix[, .(from_id, oi = to_id, travel_time)]))
  expect_error(
    tester(
      travel_matrix[, .(from_id, to_id, oi = travel_time)],
      travel_cost = "travel_time"
    )
  )
  expect_error(
    tester(
      travel_matrix[, .(from_id, to_id, travel_time, oi = mode)],
      group_by = "mode"
    )
  )

  expect_error(tester(as.list(land_use_data)))
  expect_error(
    tester(land_use_data = land_use_data[, .(oi = id, jobs)])
  )
  expect_error(
    tester(
      land_use_data = land_use_data[, .(id, oi = jobs)],
      opportunity = "jobs"
    )
  )
})

test_that("throws warning if travel_matrix has extra col", {
  # i.e. col not listed in travel_cost and by_col
  expect_warning(tester(group_by = character(0)))
})

test_that("returns a dataframe whose class is the same as travel_matrix's", {
  result <- tester()
  expect_is(result, "data.table")
  result <- tester(land_use_data = as.data.frame(land_use_data))
  expect_is(result, "data.table")

  result <- tester(as.data.frame(smaller_matrix))
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
  result <- tester(as.data.frame(smaller_matrix), as.data.frame(land_use_data))
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
})

test_that("result has correct structure", {
  result <- tester()
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$jobs, "numeric")

  result <- tester(opportunity = "schools")
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$schools, "numeric")

  suppressWarnings(result <- tester(group_by = character(0)))
  expect_true(ncol(result) == 2)
  expect_is(result$id, "character")
  expect_is(result$jobs, "numeric")

  result <- tester(
    data.table::data.table(
      mode = character(),
      from_id = character(),
      to_id = character(),
      travel_time = integer()
    )
  )
  expect_true(ncol(result) == 3)
  expect_true(nrow(result) == 0)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$jobs, "numeric")
})

test_that("input data sets remain unchanged", {
  original_smaller_matrix <- travel_matrix[1:10]
  original_land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

  result <- tester()

  # Reset all indices for comparison
  data.table::setindex(smaller_matrix, NULL)
  data.table::setindex(land_use_data, NULL)
  data.table::setindex(original_smaller_matrix, NULL)
  data.table::setindex(original_land_use_data, NULL)

  expect_equal(original_smaller_matrix, smaller_matrix)
  expect_equal(original_land_use_data, land_use_data)
})

test_that("fill_missing_ids arg works correctly", {
  small_travel_matrix <- travel_matrix[
    from_id %in% c("89a88cdb57bffff", "89a88cdb597ffff")
  ]
  small_travel_matrix <- small_travel_matrix[
    !(from_id == "89a88cdb57bffff" & mode == "transit2")
  ]

  result <- tester(small_travel_matrix, fill_missing_ids = TRUE)
  result[, jobs := as.integer(jobs)]
  data.table::setkey(result, NULL)
  expect_identical(
    result,
    data.table::data.table(
      id = rep(c("89a88cdb57bffff", "89a88cdb597ffff"), each = 2),
      mode = rep(c("transit", "transit2"), times = 2),
      jobs = c(164981L, 0L, 165553L, 165553L) # Update these expected values
    )
  )

  result <- tester(small_travel_matrix, fill_missing_ids = FALSE)
  result[, jobs := as.integer(jobs)]
  expect_identical(
    result,
    data.table::data.table(
      id = c("89a88cdb57bffff", "89a88cdb597ffff", "89a88cdb597ffff"),
      mode = c("transit", "transit", "transit2"),
      jobs = c(164981L, 165553L, 165553L) # Update these expected values
    )
  )
})

test_that("accepts custom decay function", {
  selected_ids <- c(
    "89a88cdb57bffff",
    "89a88cdb597ffff",
    "89a88cdb5b3ffff",
    "89a88cdb5cfffff",
    "89a88cd909bffff"
  )
  smaller_travel_matrix <- travel_matrix[
    from_id %in% selected_ids & to_id %in% selected_ids
  ]

  custom_function <- function(travel_cost) rep(1L, length(travel_cost))

  result <- tester(smaller_travel_matrix, decay_function = custom_function)
  result[, jobs := round(jobs, digits = 1)] #for comparision purposes
  expect_identical(
    result,
    data.table::data.table(
      id = rep(selected_ids, 2),
      mode = rep(c("transit", "transit2"), each = 5),
      jobs = rep(49608.8, 10)  # All values are the same
    )
  )
})

test_that("calculates total constrained accessibility correctly", {
  # Use the same test data from spatial_availability tests
  paper_travel_matrix <- data.table::data.table(
    from_id = rep(c("A", "B", "C"), each = 3),
    to_id = as.character(rep(1:3, 3)),
    travel_time = c(15, 30, 100, 30, 15, 100, 100, 100, 15)
  )
  paper_land_use_data <- data.table::data.table(
    id = c("A", "B", "C", "1", "2", "3"),
    jobs = c(0, 0, 0, 100000, 100000, 10000)
  )

  result <- tester(
    paper_travel_matrix,
    paper_land_use_data,
    group_by = character(0)
  )
  #result[, jobs := round(jobs, digits = 0)]

  # Test conservation property - sum should equal total opportunities
  total_jobs <- sum(paper_land_use_data[id %in% c("1", "2", "3"), jobs])
  expect_equal(sum(result$jobs), total_jobs)#, tolerance = 1)

  # Test that all origins are present
  expect_setequal(result$id, c("A", "B", "C"))
})

test_that("results are grouped by decay_function_arg when needed", {
  small_travel_matrix <- travel_matrix[
    from_id %in% c("89a88cdb57bffff", "89a88cdb597ffff") &
      mode != "transit2"
  ]

  result <- tester(
    small_travel_matrix,
    decay_function = decay_exponential(c(0.5, 0.6))
  )
  #result[, jobs := round(jobs, 1)]

  expect_true("decay_function_arg" %in% names(result))
  expect_equal(unique(result$decay_function_arg), c(0.5, 0.6))
})

test_that("throws warning w/ fill_missing_ids = FALSE with detailed_results", {
  expect_warning(tester(fill_missing_ids = FALSE, detailed_results = TRUE))
})

test_that("result has correct structure with detailed_results = TRUE", {
  result <- tester(detailed_results = TRUE)
  expect_true(ncol(result) >= 5) # from_id, to_id, group_cols, kappa_tot, jobs
  expect_is(result$mode, "character")
  expect_is(result$from_id, "character")
  expect_is(result$to_id, "character")
  expect_is(result$kappa_tot, "numeric")
  expect_is(result$jobs, "numeric")

  result <- tester(detailed_results = TRUE, opportunity = "schools")
  expect_true(ncol(result) >= 5)
  expect_is(result$mode, "character")
  expect_is(result$from_id, "character")
  expect_is(result$to_id, "character")
  expect_is(result$kappa_tot, "numeric")
  expect_is(result$schools, "numeric")

  result <- tester(smaller_matrix[0], detailed_results = TRUE)
  expect_true(ncol(result) >= 5)
  expect_true(nrow(result) == 0)
  expect_is(result$mode, "character")
  expect_is(result$from_id, "character")
  expect_is(result$to_id, "character")
  expect_is(result$kappa_tot, "numeric")
  expect_is(result$jobs, "numeric")
})
