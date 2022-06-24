context("Cumulative cutoff function")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           cutoff = 20,
                           opportunity_col = 'schools',
                           travel_cost_col = 'travel_time',
                           by_col='from_id') {

  results <- accessibility::cumulative_time_cutoff(data = data,
                                      cutoff = cutoff,
                                      opportunity_col = opportunity_col,
                                      travel_cost_col = travel_cost_col,
                                      by_col = by_col)
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # vars with col names do not exist in data input
  expect_error(default_tester(opportunity_col = 'banana'))
  expect_error(default_tester(by_col = 'banana'))
  expect_error(default_tester(travel_cost_col = 'banana'))
  expect_error(default_tester(opportunity_col = 999))
  expect_error(default_tester(by_col = 999))
  expect_error(default_tester(travel_cost_col = 999))

  # cutoff value is not positive numeric
  expect_error(default_tester(cutoff = "banana"))
  expect_error(default_tester(cutoff = -3))
  expect_error(is(default_tester(cutoff = Inf), "data.table"))

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
  expect_is( default_tester(cutoff = 1), "data.table")
  expect_is( default_tester(cutoff = 1000), "data.table")

})
