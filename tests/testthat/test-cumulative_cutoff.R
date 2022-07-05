# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(
  travel_matrix = get("travel_matrix", envir = parent.frame()),
  land_use_data = get("land_use_data", envir = parent.frame()),
  cutoff = 30,
  opportunity_col = "jobs",
  travel_cost_col = "travel_time",
  by_col = "mode",
  active = TRUE
) {
  cumulative_time_cutoff(
    travel_matrix,
    land_use_data,
    cutoff,
    opportunity_col,
    travel_cost_col,
    by_col,
    active
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(cutoff = "banana"))
  expect_error(tester(cutoff = -3))
  expect_error(tester(cutoff = c(1, 1)))
  expect_error(tester(cutoff = Inf))

  expect_error(tester(opportunity_col = 1))
  expect_error(tester(opportunity_col = c("schools", "jobs")))

  expect_error(tester(travel_cost_col = 1))
  expect_error(tester(travel_cost_col = c("travel_time", "monetary_cost")))

  expect_error(tester(by_col = 1))
  expect_error(tester(by_col = c("mode", "departure_time")))
  expect_error(tester(by_col = "from_id"))

  expect_error(tester(as.list(travel_matrix)))
  expect_error(tester(travel_matrix[, .(oi = from_id, to_id, travel_time)]))
  expect_error(tester(travel_matrix[, .(from_id, oi = to_id, travel_time)]))
  expect_error(
    tester(
      travel_matrix[, .(from_id, to_id, oi = travel_time)],
      travel_cost_col = "travel_time"
    )
  )
  expect_error(
    tester(
      travel_matrix[, .(from_id, to_id, travel_time, oi = mode)],
      by_col = "mode"
    )
  )

  expect_error(tester(as.list(land_use_data)))
  expect_error(tester(land_use_data = land_use_data[, .(oi = id, jobs)]))
  expect_error(
    tester(
      land_use_data = land_use_data[, .(id, oi = jobs)],
      opportunity_col = "jobs"
    )
  )
})

test_that("throws warning if travel_matrix extra col", {
  # i.e. col not listed in travel_cost_col and by_col
  expect_warning(tester(by_col = NULL))
})

test_that("returns a dataframe whose class is the same as travel_matrix's", {
  result <- tester()
  expect_is(result, "data.table")
  result <- tester(land_use_data = as.data.frame(land_use_data))
  expect_is(result, "data.table")

  result <- tester(as.data.frame(travel_matrix))
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
  result <- tester(
    as.data.frame(travel_matrix),
    land_use_data = as.data.frame(land_use_data)
  )
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
})

test_that("result has correct structure", {
  result <- tester()
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$jobs, "integer")

  result <- tester(opportunity_col = "schools")
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$schools, "integer")

  suppressWarnings(result <- tester(by_col = NULL))
  expect_true(ncol(result) == 2)
  expect_is(result$id, "character")
  expect_is(result$jobs, "integer")
})

test_that("input data sets remain unchanged", {
  original_travel_matrix <- data.table::rbindlist(
    travel_matrix_list,
    idcol = "mode"
  )
  original_land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

  result <- tester()

  expect_identical(original_travel_matrix, travel_matrix)
  expect_identical(original_land_use_data, land_use_data)
})

test_that("active and passive accessibility is correctly calculated", {
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

  result <- tester(smaller_travel_matrix, by_col = "mode")
  expected_result <- data.table::data.table(
    id = rep(selected_ids, 2),
    mode = rep(c("transit", "transit2"), each = 5),
    jobs = rep(as.integer(c(82, 408, 408, 109, 0)), 2)
  )
  expect_identical(result, expected_result)

  result <- tester(
    smaller_travel_matrix,
    cutoff = 45,
    opportunity_col = "population",
    by_col = "mode",
    active = FALSE
  )
  expected_result <- data.table::data.table(
    id = rep(selected_ids, 2),
    mode = rep(c("transit", "transit2"), each = 5),
    population = rep(as.integer(c(4874, 4268, 2404, 4268, 29)), 2)
  )
  expect_identical(result, expected_result)
})
