# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(
  travel_matrix = get("travel_matrix", envir = parent.frame()),
  land_use_data = get("land_use_data", envir = parent.frame()),
  decay_function = decay_binary(30),
  opportunity_col = "jobs",
  travel_cost_col = "travel_time",
  by_col = "mode",
  active = TRUE,
  fill_missing_ids = TRUE
) {
  gravity(
    travel_matrix,
    land_use_data,
    decay_function,
    opportunity_col,
    travel_cost_col,
    by_col,
    active,
    fill_missing_ids
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(decay_function = "a"))
  expect_error(tester(decay_function = mean))
  expect_error(tester(decay_function = get))

  expect_error(tester(opportunity_col = 1))
  expect_error(tester(opportunity_col = c("schools", "jobs")))

  expect_error(tester(travel_cost_col = 1))
  expect_error(tester(travel_cost_col = c("travel_time", "monetary_cost")))

  expect_error(tester(by_col = 1))
  expect_error(tester(by_col = c("mode", "departure_time")))
  expect_error(tester(by_col = "from_id"))

  expect_error(tester(active = 1))
  expect_error(tester(active = c(TRUE, TRUE)))
  expect_error(tester(active = NA))

  expect_error(tester(fill_missing_ids = 1))
  expect_error(tester(fill_missing_ids = c(TRUE, TRUE)))
  expect_error(tester(fill_missing_ids = NA))

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
  expect_is(result$jobs, "integer")
})

test_that("input data sets remain unchanged", {
  original_travel_matrix <- data.table::rbindlist(
    travel_matrix_list,
    idcol = "mode"
  )
  original_land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

  result <- tester()

  # subsets in other functions tests set travel_matrix index
  data.table::setindex(travel_matrix, NULL)

  expect_identical(original_travel_matrix, travel_matrix)
  expect_identical(original_land_use_data, land_use_data)
})

test_that("active and passive accessibility is correctly calculated", {
  # using decay_binary() should yield exact same results of cumulative_cutoff()
  # active/passive cumulative_cutoff() is tested in its own file
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

  result <- tester(smaller_travel_matrix)
  cum_result <- cumulative_cutoff(
    smaller_travel_matrix,
    land_use_data,
    cutoff = 30,
    opportunity_col = "jobs",
    travel_cost_col = "travel_time",
    by_col = "mode"
  )
  expect_identical(result, cum_result)

  result <- tester(smaller_travel_matrix, active = FALSE)
  cum_result <- cumulative_cutoff(
    smaller_travel_matrix,
    land_use_data,
    cutoff = 30,
    opportunity_col = "jobs",
    travel_cost_col = "travel_time",
    by_col = "mode",
    active = FALSE
  )
  expect_identical(result, cum_result)
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

  expected_result <- smaller_travel_matrix[
    land_use_data,
    on = c(to_id = "id"),
    jobs := i.jobs
  ]
  expected_result <- expected_result[
    ,
    .(jobs = sum(jobs)),
    by = .(id = from_id, mode)
  ]

  expect_identical(result, expected_result)
})

test_that("fill_missing_ids arg works correctly", {
  small_travel_matrix <- travel_matrix[
    from_id %in% c("89a88cdb57bffff", "89a88cdb597ffff")
  ]
  small_travel_matrix <- small_travel_matrix[
    !(from_id == "89a88cdb57bffff" & mode == "transit2")
  ]

  result <- tester(
    small_travel_matrix,
    land_use_data,
    fill_missing_ids = TRUE
  )
  data.table::setkey(result, NULL)
  expect_identical(
    result,
    data.table::data.table(
      id = rep(c("89a88cdb57bffff", "89a88cdb597ffff"), each = 2),
      mode = rep(c("transit", "transit2"), times = 2),
      jobs = c(22239L, 0L, 36567L, 36567L)
    )
  )

  result <- tester(
    small_travel_matrix,
    land_use_data,
    fill_missing_ids = FALSE
  )
  expect_identical(
    result,
    data.table::data.table(
      id = c("89a88cdb57bffff", "89a88cdb597ffff", "89a88cdb597ffff"),
      mode = c("transit", "transit", "transit2"),
      jobs = c(22239L, 36567L, 36567L)
    )
  )
})