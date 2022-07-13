testthat::skip("skipping for now")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

default_tester <- function(data = ttm,
                           opportunity_col = 'schools',
                           decay_function = decay_linear(cutoff = 50),
                           travel_cost_col = 'travel_time',
                           by_col='from_id') {

  results <- accessibility::gravity_access(data = data,
                                      opportunity_col = opportunity_col,
                                      decay_function= decay_function,
                                      travel_cost_col = travel_cost_col,
                                      by_col = by_col)
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # vars with col names do not exist in data input
  expect_error(default_tester(by_col = 999))
  expect_error(default_tester(travel_cost_col = 999))
  expect_error(default_tester(opportunity_col = 999))
  expect_error(default_tester(by_col = 'banana'))
  expect_error(default_tester(travel_cost_col = 'banana'))
  expect_error(default_tester(opportunity_col = 'banana'))

  # not a decay function
  expect_error(default_tester(decay_function = 999))

})


# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values

  # different opportunity_col
  expect_is( default_tester(opportunity_col = 'population'), "data.table")

  # different by_col
  expect_is( default_tester(by_col = 'from_id'), "data.table")

  # different cutoff values
  expect_is( default_tester(decay_function= decay_linear(cutoff = 50)), "data.table")
  expect_is( default_tester(decay_function= decay_exponential(decay_value = 0.5)), "data.table")

})
