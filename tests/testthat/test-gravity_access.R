context("Gravity access")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_col = 'schools',
                           decay_function = 'linear',
                           cutoff = 30,
                           decay_value=0.5,
                           by_col='from_id') {

  results <- accessibility::gravity_access(data = data,
                                      opportunity_col = opportunity_col,
                                      cutoff = cutoff,
                                      decay_function= decay_function,
                                      decay_value=decay_value,
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
