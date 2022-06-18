context("Cumulative cutoff function")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_colname = 'schools',
                           cutoff = 20,
                           by_colname='from_id') {

  results <- accessibility::cumulative_time_cutoff(data = data,
                                      opportunity_colname = opportunity_colname,
                                      cutoff = cutoff,
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

  # cutoff value is not positive numeric
  expect_error(default_tester(cutoff = "banana"))
  expect_error(default_tester(cutoff = -3))
  expect_error(is(default_tester(cutoff = Inf), "data.table"))

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
