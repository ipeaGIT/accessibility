context("Cumulative interval function")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_colname = 'schools',
                           start = 20,
                           end = 25,
                           stat = 'mean',
                           by_colname='from_id') {

  results <- accessibility::cumulative_time_interval(data = data,
                                      opportunity_colname = opportunity_colname,
                                      start = start,
                                      end = end,
                                      stat = stat,
                                      by_colname = by_colname)
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # opportunity_colname and by_colname do not exist in data input
  expect_error(default_tester(opportunity_colname = 'banana'))
  expect_error(default_tester(by_colname = 'banana'))
  expect_error(default_tester(opportunity_colname = 999))
  expect_error(default_tester(by_colname = 999))

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

  # # output class
  # expect_true(is(default_tester(), "data.table"))

  # different opportunity_colname
  expect_true(is(default_tester(opportunity_colname = 'population'), "data.table"))

  # different by_colname
  expect_true(is(default_tester(by_colname = 'from_id'), "data.table"))

  # different interval values
  expect_true(is(default_tester(start = 1), "data.table"))
  expect_true(is(default_tester(end = 60), "data.table"))
  expect_true(is(default_tester(start=30, end = 20), "data.table"))

  # different summary stat
  expect_true(is(default_tester(stat = 'median'), "data.table"))

})
