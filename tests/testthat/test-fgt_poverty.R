# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(accessibility_data = small_access,
                   sociodemographic_data = land_use_data,
                   opportunity = "jobs",
                   population = "population",
                   poverty_line = 10000,
                   group_by = "mode") {
  fgt_poverty(
    accessibility_data,
    sociodemographic_data,
    opportunity,
    population,
    poverty_line,
    group_by
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(opportunity = 1))
  expect_error(tester(opportunity = c("schools", "jobs")))

  expect_error(tester(population = 1))
  expect_error(tester(population = c("schools", "jobs")))

  expect_error(tester(poverty_line = "a"))
  expect_error(tester(poverty_line = -1))

  expect_error(tester(group_by = 1))
  expect_error(tester(group_by = NA))
  expect_error(tester(group_by = "id"))
  expect_error(tester(group_by = c("mode", "mode")))

  expect_error(tester(as.list(small_access)))
  expect_error(tester(small_access[, .(oi = id, jobs, mode)]))
  expect_error(tester(small_access[, .(id, oi = jobs, mode)]))
  expect_error(tester(small_access[, .(id, jobs, oi = mode)]))

  expect_error(tester(sociodemographic_data = as.list(land_use_data)))
  expect_error(
    tester(sociodemographic_data = land_use_data[, .(oi = id, population)])
  )
  expect_error(
    tester(sociodemographic_data = land_use_data[, .(id, oi = population)])
  )
})

test_that("throws warning if accessibility_data has an extra col", {
  expect_warning(tester(group_by = character(0)))
})

test_that("returns a dataframe with same class as accessibility_data's", {
  result <- tester()
  expect_is(result, "data.table")
  result <- tester(sociodemographic_data = as.data.frame(land_use_data))
  expect_is(result, "data.table")

  result <- tester(as.data.frame(small_access))
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
  result <- tester(
    as.data.frame(small_access),
    sociodemographic_data = as.data.frame(land_use_data)
  )
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
})

test_that("result has correct structure", {
  result <- tester()
  expect_true(ncol(result) == 4)
  expect_is(result$mode, "character")
  expect_is(result$FGT0, "numeric")
  expect_is(result$FGT1, "numeric")
  expect_is(result$FGT2, "numeric")

  suppressWarnings(result <- tester(group_by = character(0)))
  expect_true(ncol(result) == 3)
  expect_is(result$FGT0, "numeric")
  expect_is(result$FGT1, "numeric")
  expect_is(result$FGT2, "numeric")

  result <- tester(small_access[0])
  expect_true(nrow(result) == 0)
  expect_true(ncol(result) == 4)
  expect_is(result$mode, "character")
  expect_is(result$FGT0, "numeric")
  expect_is(result$FGT1, "numeric")
  expect_is(result$FGT2, "numeric")

  suppressWarnings(result <- tester(small_access[0], group_by = character(0)))
  expect_true(nrow(result) == 0)
  expect_true(ncol(result) == 3)
  expect_is(result$FGT0, "numeric")
  expect_is(result$FGT1, "numeric")
  expect_is(result$FGT2, "numeric")
})

test_that("input data sets remain unchanged", {
  original_access_data <- cumulative_cutoff(
    smaller_matrix,
    land_use_data,
    opportunity = "jobs",
    travel_cost = "travel_time",
    cutoff = 30,
    group_by = "mode"
  )
  original_sociodem_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

  result <- tester()

  expect_equal(original_access_data, small_access)
  expect_equal(original_sociodem_data, land_use_data)
})

test_that("poverty levels are correctly calculated", {
  selected_ids <- c(
    "89a88cd909bffff",
    "89a88cdb57bffff",
    "89a88cdb597ffff",
    "89a88cdb5b3ffff",
    "89a88cdb5cfffff"
  )
  access_data <- cumulative_cutoff(
    travel_matrix[from_id %in% selected_ids],
    land_use_data,
    opportunity = "jobs",
    travel_cost = "travel_time",
    cutoff = 30,
    group_by = "mode"
  )

  # everything is 0 when all access levels are above the poverty line (10000)

  result <- tester(access_data)
  expected_result <- data.table::data.table(
    mode = c("transit", "transit2"),
    FGT0 = 0,
    FGT1 = 0,
    FGT2 = 0
  )
  expect_identical(result, expected_result)

  # FGT0 is 1 when all access levels are below poverty lines

  result <- tester(access_data, poverty_line = 100000)
  result[, `:=`(FGT1 = round(FGT1, 4), FGT2 = round(FGT2, 4))]
  expected_result <- data.table::data.table(
    mode = c("transit", "transit2"),
    FGT0 = 1,
    FGT1 = 0.5588,
    FGT2 = 0.3256
  )
  expect_identical(result, expected_result)

  access_data <- access_data[!(id == "89a88cdb5cfffff" & mode == "transit2")]
  result <- tester(access_data, poverty_line = 100000)
  result[, `:=`(FGT1 = round(FGT1, 4), FGT2 = round(FGT2, 4))]
  expected_result <- data.table::data.table(
    mode = c("transit", "transit2"),
    FGT0 = 1,
    FGT1 = c(0.5588, 0.6367),
    FGT2 = c(0.3256, 0.4129)
  )
  expect_identical(result, expected_result)
})

test_that("works even if access_data and sociodem_data has specific colnames", {
  selected_ids <- c(
    "89a88cdb57bffff",
    "89a88cdb5b3ffff"
  )
  access_data <- cumulative_cutoff(
    travel_matrix[from_id %in% selected_ids],
    land_use_data,
    opportunity = "jobs",
    travel_cost = "travel_time",
    cutoff = 30,
    group_by = "mode"
  )
  expected_result <- tester(access_data)

  access_data[, opportunity := "oi"]
  result <- suppressWarnings(tester(access_data))
  expect_identical(expected_result, result)

  access_data[, opportunity := NULL]
  land_use_data[, population_temp := population]
  land_use_data[, population := 1]
  result <- tester(access_data, population = "population_temp")
  expect_identical(expected_result, result)

  land_use_data[, population := population_temp]
  land_use_data[, population_temp := NULL]
  access_data[, group_by := "oi"]
  result <- suppressWarnings(tester(access_data))
  expect_identical(expected_result, result)

  access_data[, group_by := NULL]
})
