# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

# creating this copy of travel_matrix because apply_gravity_measure() can modify
# the original object by reference
data <- data.table::as.data.table(travel_matrix)
data[, mode := NULL]
data <- unique(data)

tester <- function(data = get("data", envir = parent.frame()),
                   decay_function = decay_exponential(0.5),
                   travel_cost = "travel_time") {
  apply_gravity_measure(data, decay_function, travel_cost)
}

legacy_decay_exponential <- function(decay_value) {
  function(travel_cost) {
    weights <- exp(-decay_value * travel_cost)
    return(weights)
  }
}

test_that("mod by ref when input fn returns num vector or list with length 1", {
  expect_named(data, c("from_id", "to_id", "travel_time"))

  result <- tester()
  expect_identical(result, data)
  expect_named(data, c("from_id", "to_id", "travel_time", "opp_weight"))
  data[, opp_weight := NULL]

  result <- tester(decay_function = legacy_decay_exponential(0.5))
  expect_identical(result, data)
  expect_named(data, c("from_id", "to_id", "travel_time", "opp_weight"))
  data[, opp_weight := NULL]
})

test_that("doesnt mod by ref when input returns list length > 1 and add col", {
  expect_named(data, c("from_id", "to_id", "travel_time"))

  result <- tester(decay_function = decay_exponential(c(0.5, 0.6)))
  expected_result <- cbind(
    decay_function_arg = rep(c(0.5, 0.6), each = nrow(data)),
    rbind(data, data),
    opp_weight = c(
      legacy_decay_exponential(0.5)(data$travel_time),
      legacy_decay_exponential(0.6)(data$travel_time)
    )
  )
  expect_identical(result, expected_result)

  expect_named(data, c("from_id", "to_id", "travel_time"))
})
