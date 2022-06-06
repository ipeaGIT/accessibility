context("Minimum travel time to closest opportunity")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_colname = 'schools',
                           by_colname='from_id') {

  results <- accessibility::time_to_closest(data = data,
                                      opportunity_colname = opportunity_colname,
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

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # # output class
  # expect_true(is(default_tester(), "data.table"))

  # different opportunity_colname
  expect_true(is(default_tester(opportunity_colname = 'population'), "data.table"))

  # different by_colname
  expect_true(is(default_tester(by_colname = 'from_id'), "data.table"))

})
