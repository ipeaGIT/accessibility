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
  cost_to_closest(
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
  expect_is(result$travel_time, "numeric")
  expect_is(result$destination, "character")

  result <- tester(active = FALSE)
  expect_true(ncol(result) == 4)
  expect_is(result$id, "character")
  expect_is(result$mode, "character")
  expect_is(result$travel_time, "numeric")
  expect_is(result$origin, "character")

  suppressWarnings(result <- tester(by_col = NULL))
  expect_true(ncol(result) == 3)
  expect_is(result$id, "character")
  expect_is(result$travel_time, "numeric")
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
  expect_is(result$travel_time, "integer")
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

test_that("active and passive accessibility is correctly calculated: n = 1", {
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
    travel_time = rep(c(47, 5.8, 13, 43, 70), 2),
    destination = "89a88cdb597ffff"
  )
  expect_identical(result, expected_result)

  result <- tester(
    smaller_travel_matrix,
    opportunity_col = "population",
    by_col = "mode",
    active = FALSE
  )
  expected_result <- data.table::data.table(
    id = rep(selected_ids, 2),
    mode = rep(c("transit", "transit2"), each = 5),
    travel_time = 5.8,
    origin = rep(selected_ids, 2)
  )
  expect_identical(result, expected_result)
})

test_that("output is as expected when ids are missing - n=1", {
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
  smaller_travel_matrix <- rbind(
    smaller_travel_matrix,
    data.table::data.table(
      mode = "transit",
      from_id = "fake_id",
      to_id = "89a88cdb57bffff",
      travel_time = 50
    )
  )

  result <- tester(smaller_travel_matrix, n = 1)
  fake_id_result <- result[id == "fake_id"]
  data.table::setkey(fake_id_result, NULL)
  expected_result <- data.table::data.table(
    id = "fake_id",
    mode = c("transit", "transit2"),
    travel_time = Inf,
    destination = NA_character_
  )
  expect_identical(fake_id_result, expected_result)

  result <- tester(smaller_travel_matrix, n = 1, fill_missing_ids = FALSE)
  fake_id_result <- result[id == "fake_id"]
  data.table::setkey(fake_id_result, NULL)
  expected_result <- data.table::data.table(
    id = character(0),
    mode = character(0),
    travel_time = numeric(0),
    destination = character(0)
  )
  expect_identical(fake_id_result, expected_result)
})

test_that("output is as expected when ids are missing - n>1", {
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
  smaller_travel_matrix <- smaller_travel_matrix[
    !(from_id == "89a88cd909bffff" & to_id == "89a88cdb597ffff")
  ]

  result <- tester(smaller_travel_matrix, n = 2)
  missing_id_result <- result[id == "89a88cd909bffff"]
  data.table::setkey(missing_id_result, NULL)
  expected_result <- data.table::data.table(
    id = "89a88cd909bffff",
    mode = c("transit", "transit2"),
    travel_time = Inf,
    destination = NA_character_
  )
  expect_identical(missing_id_result, expected_result)

  result <- tester(smaller_travel_matrix, n = 2, fill_missing_ids = FALSE)
  missing_id_result <- result[id == "89a88cd909bffff"]
  data.table::setkey(missing_id_result, NULL)
  expected_result <- data.table::data.table(
    id = character(0),
    mode = character(0),
    travel_time = numeric(0),
    destination = character(0)
  )
  expect_identical(missing_id_result, expected_result)
})
