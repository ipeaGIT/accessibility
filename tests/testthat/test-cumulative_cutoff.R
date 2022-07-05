# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(
  travel_matrix = get("travel_matrix", envir = parent.frame()),
  land_use_data = get("land_use_data", envir = parent.frame()),
  cutoff = 30,
  opportunity_col = "jobs",
  travel_cost_col = "travel_time",
  by_col = NULL,
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
