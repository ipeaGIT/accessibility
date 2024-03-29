# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

fast_smaller_matrix <- data.table::copy(smaller_matrix)
fast_smaller_matrix[, travel_time := travel_time / 4]

tester <- function(travel_matrix = fast_smaller_matrix,
                   land_use_data = get("land_use_data", envir = parent.frame()),
                   opportunity = "jobs",
                   travel_cost = "travel_time",
                   demand = "population",
                   cost_increment = 1,
                   group_by = "mode",
                   fill_missing_ids = TRUE) {
  balancing_cost(
    travel_matrix,
    land_use_data,
    opportunity,
    travel_cost,
    demand,
    cost_increment,
    group_by,
    fill_missing_ids
  )
}

test_that("raises errors due to incorrect input", {
  expect_error(tester(opportunity = 1))
  expect_error(tester(opportunity = c("schools", "jobs")))

  expect_error(tester(travel_cost = 1))
  expect_error(tester(travel_cost = c("travel_time", "monetary_cost")))

  expect_error(tester(demand = 1))
  expect_error(tester(demand = c("population", "population")))

  expect_error(tester(cost_increment = "a"))
  expect_error(tester(cost_increment = -1))
  expect_error(tester(cost_increment = Inf))

  expect_error(tester(group_by = 1))
  expect_error(tester(group_by = NA))
  expect_error(tester(group_by = "from_id"))
  expect_error(tester(group_by = c("mode", "mode")))

  expect_error(tester(fill_missing_ids = 1))
  expect_error(tester(fill_missing_ids = c(TRUE, TRUE)))
  expect_error(tester(fill_missing_ids = NA))

  expect_error(tester(as.list(travel_matrix)))
  expect_error(
    tester(travel_matrix[, .(oi = from_id, to_id, travel_time, mode)])
  )
  expect_error(
    tester(travel_matrix[, .(from_id, oi = to_id, travel_time, mode)])
  )
  expect_error(
    tester(travel_matrix[, .(from_id, to_id, oi = travel_time, mode)])
  )
  expect_error(
    tester(travel_matrix[, .(from_id, to_id, travel_time, oi = mode)])
  )

  expect_error(tester(as.list(land_use_data)))
  expect_error(
    tester(land_use_data = land_use_data[, .(oi = id, jobs, population)])
  )
  expect_error(
    tester(land_use_data = land_use_data[, .(id, oi = jobs, population)])
  )
  expect_error(
    tester(land_use_data = land_use_data[, .(id, jobs, oi = population)])
  )
})

test_that("throws warning if travel_matrix extra col", {
  # i.e. col not listed in travel_cost and by_col
  expect_warning(tester(group_by = character(0)))
})

test_that("returns a dataframe whose class is the same as travel_matrix's", {
  result <- tester()
  expect_is(result, "data.table")
  result <- tester(land_use_data = as.data.frame(land_use_data))
  expect_is(result, "data.table")

  result <- tester(as.data.frame(fast_smaller_matrix))
  expect_false(inherits(result, "data.table"))
  expect_is(result, "data.frame")
  result <- tester(
    as.data.frame(fast_smaller_matrix),
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
  expect_is(result$travel_time, "numeric")

  suppressWarnings(result <- tester(group_by = character(0)))
  expect_true(ncol(result) == 2)
  expect_is(result$id, "character")
  expect_is(result$travel_time, "numeric")

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
  expect_is(result$travel_time, "numeric")
})

test_that("input data sets remain unchanged", {
  original_fast_smaller_matrix <- travel_matrix[1:10]
  original_fast_smaller_matrix[, travel_time := travel_time / 4]
  original_land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

  result <- tester()

  expect_identical(original_fast_smaller_matrix, fast_smaller_matrix)
  expect_identical(original_land_use_data, land_use_data)
})

test_that("calculates balancing cost correctly", {
  selected_ids <- c(
    "89a88cda64fffff",
    "89a88cdb027ffff",
    "89a88cdb12bffff",
    "89a88cdb287ffff",
    "89a88cdb67bffff"
  )
  smaller_ttm <- travel_matrix[
    from_id %in% selected_ids & to_id %in% selected_ids
  ]
  smaller_ttm[mode == "transit2", travel_time := travel_time + 10]

  result <- tester(smaller_ttm)
  expected_result <- data.table::data.table(
    id = rep(selected_ids, each = 2),
    mode = rep(c("transit", "transit2"), 5),
    travel_time = c(42, 52, 6, 16, 27, 37, 6, 16, 20, 30)
  )
  expect_identical(result, expected_result)
})

test_that("fill_missing_ids arg works correctly", {
  test_matrix <- rbind(
    fast_smaller_matrix,
    fast_smaller_matrix[1][, mode := "transit2"]
  )

  result <- tester(test_matrix)
  data.table::setkeyv(result, NULL)
  expect_identical(
    result,
    data.table::data.table(
      id = rep("89a88cdb57bffff", 2),
      mode = c("transit", "transit2"),
      travel_time = c(12, NA)
    )
  )

  result <- tester(test_matrix, fill_missing_ids = FALSE)
  expect_identical(
    result,
    data.table::data.table(
      id = "89a88cdb57bffff",
      mode = "transit",
      travel_time = 12
    )
  )
})

test_that("cost_increment arg works correctly", {
  selected_ids <- c(
    "89a88cda64fffff",
    "89a88cdb027ffff",
    "89a88cdb12bffff",
    "89a88cdb287ffff",
    "89a88cdb67bffff"
  )
  smaller_ttm <- travel_matrix[
    from_id %in% selected_ids & to_id %in% selected_ids
  ]

  result <- tester(smaller_ttm)
  expect_false(all(result$travel_time %% 2 == 0))

  result <- tester(smaller_ttm, cost_increment = 2)
  expect_true(all(result$travel_time %% 2 == 0))
})

test_that("works even if travel_matrix and land_use has specific colnames", {
  expected_result <- tester()

  fast_smaller_matrix[, opportunity := "oi"]
  result <- suppressWarnings(tester(fast_smaller_matrix))
  expect_identical(expected_result, result)

  fast_smaller_matrix[, opportunity := NULL]
  fast_smaller_matrix[, travel_cost := "oi"]
  result <- suppressWarnings(tester(fast_smaller_matrix))
  expect_identical(expected_result, result)

  fast_smaller_matrix[, travel_cost := NULL]
  fast_smaller_matrix[, groups := "oi"]
  result <- suppressWarnings(tester(fast_smaller_matrix))
  expect_identical(expected_result, result)

  fast_smaller_matrix[, groups := NULL]
  fast_smaller_matrix[, demand := "oi"]
  result <- suppressWarnings(tester(fast_smaller_matrix))
  expect_identical(expected_result, result)

  fast_smaller_matrix[, demand := NULL]
  land_use_data[, opportunity := "oi"]
  result <- suppressWarnings(tester(land_use_data = land_use_data))
  expect_identical(expected_result, result)

  land_use_data[, opportunity := NULL]
  land_use_data[, demand := "oi"]
  result <- suppressWarnings(tester(land_use_data = land_use_data))
  expect_identical(expected_result, result)

  land_use_data[, demand := NULL]
})
