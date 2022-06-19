context("Minimum travel time to closest opportunity")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_col = 'schools',
                           by_col='from_id',
                           n_opportunities = 1) {

  results <- accessibility::time_to_closest(data = data,
                                      opportunity_col = opportunity_col,
                                      by_col = by_col,
                                      n_opportunities = n_opportunities)
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

  # n_opportunities value is not positive numeric
  expect_error(default_tester(n_opportunities = "banana"))
  expect_error(default_tester(n_opportunities = -3))
  expect_error(is(default_tester(n_opportunities = Inf), "data.table"))
})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values

  # different opportunity_col
  expect_is( default_tester(opportunity_col = 'population'), "data.table")

  # different by_col
  expect_is( default_tester(by_col = 'from_id'), "data.table")

  # different n_opportunities values
  expect_is( default_tester(n_opportunities = 2), "data.table")
  expect_is( default_tester(n_opportunities = 3), "data.table")
})
