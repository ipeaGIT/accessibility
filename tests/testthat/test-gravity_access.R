context("Gravity access")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_colname = 'schools',
                           decay_function = 'linear',
                           cutoff = 60,
                           decay_value=0.5,
                           by_colname='from_id') {

  results <- accessibility::gravity_access(data = data,
                                      opportunity_colname = opportunity_colname,
                                      cutoff = cutoff,
                                      decay_function= decay_function,
                                      decay_value=decay_value,
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

  # TO DO:
  #> test output values

  # different opportunity_colname
  expect_is( default_tester(opportunity_colname = 'population'), "data.table")

  # different by_colname
  expect_is( default_tester(by_colname = 'from_id'), "data.table")

  # different cutoff values
  expect_is( default_tester(cutoff = 1), "data.table")
  expect_is( default_tester(cutoff = 1000), "data.table")

})
