context("Cumulative interval function")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_col = 'schools',
                           start = 20,
                           end = 25,
                           stat = 'mean',
                           by_col='from_id') {

  results <- accessibility::cumulative_time_interval(data = data,
                                      opportunity_col = opportunity_col,
                                      start = start,
                                      end = end,
                                      stat = stat,
                                      by_col = by_col)
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # opportunity_col and by_col do not exist in data input
  expect_error(default_tester(opportunity_col = 'banana'))
  expect_error(default_tester(by_col = 'banana'))
  expect_error(default_tester(opportunity_col = 999))
  expect_error(default_tester(by_col = 999))

  # invalid summary stat
  expect_error(default_tester(stat = 'banana'))
  expect_error(default_tester(stat = 999))


  # start / end values are not positive numeric
  expect_error(default_tester(start = "banana"))
  expect_error(default_tester(end = "banana"))
  expect_error(default_tester(start = -3))
  expect_error(default_tester(end = -3))
  expect_error(is(default_tester(start = Inf), "data.table"))
  expect_error(is(default_tester(end = Inf), "data.table"))

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values


  # different opportunity_col
  expect_is( default_tester(opportunity_col = 'population'), "data.table")

  # different by_col
  expect_is( default_tester(by_col = 'from_id'), "data.table")

  # different interval values
  expect_is( default_tester(start = 1), "data.table")
  expect_is( default_tester(end = 60), "data.table")
  expect_is( default_tester(start=30, end = 20), "data.table")

  # different summary stat
  expect_is( default_tester(stat = 'median'), "data.table")

})
