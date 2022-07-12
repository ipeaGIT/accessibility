# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(
  travel_matrix = get("travel_matrix", envir = parent.frame()),
  land_use_data = get("land_use_data", envir = parent.frame()),
  n = 1,
  opportunity_col = "schools",
  travel_cost_col = "travel_time",
  by_col = "mode",
  active = TRUE,
  fill_missing_ids = TRUE
) {
  time_to_closest(
    travel_matrix,
    land_use_data,
    n,
    opportunity_col,
    travel_cost_col,
    by_col,
    active,
    fill_missing_ids
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(n = "banana"))
  expect_error(tester(n = -3))
  expect_error(tester(n = c(1, 1)))
  expect_error(tester(n = Inf))

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
  expect_true(ncol(result) == 4)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$schools, "numeric")
  expect_is(result$destination, "character")

  result <- tester(opportunity_col = "jobs")
  expect_true(ncol(result) == 4)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$jobs, "numeric")
  expect_is(result$destination, "character")

  suppressWarnings(result <- tester(by_col = NULL))
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$schools, "numeric")
  expect_is(result$destination, "character")

  result <- tester(
    data.table::data.table(
      mode = character(),
      from_id = character(),
      to_id = character(),
      travel_time = integer()
    )
  )
  expect_true(ncol(result) == 4)
  expect_true(nrow(result) == 0)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$schools, "numeric")
  expect_is(result$destination, "character")
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
  # TODO: add tests
})

test_that("output is as expected when ids are missing", {
  # TODO: add tests
})
