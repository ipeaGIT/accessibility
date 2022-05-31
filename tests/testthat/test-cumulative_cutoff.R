context("Cumulative cutoff function")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           opportunity_colname = 'schools',
                           cutoff = 20,
                           by_col='from_id') {

  results <- accessibility::cumulative_time_threshold(data = data,
                                      opportunity_colname = opportunity_colname,
                                      cutoff = cutoff,
                                      by_col = by_col)
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # opportunity_colname and by_col do not exist in data input
  expect_error(default_tester(opportunity_colname = 'banana'))
  expect_error(default_tester(by_col = 'banana'))
  expect_error(default_tester(opportunity_colname = 999))
  expect_error(default_tester(by_col = 999))

  # cutoff value is not positive numeric
  expect_error(default_tester(cutoff = "banana"))
  expect_error(default_tester(cutoff = -3))

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # # output class
  # expect_true(is(default_tester(), "data.table"))

  # different opportunity_colname
  expect_true(is(default_tester(opportunity_colname = 'population'), "data.table"))

  # different by_col
  expect_true(is(default_tester(by_col = 'from_id'), "data.table"))

  # different cutoff values
  expect_true(is(default_tester(cutoff = Inf), "data.table"))
  expect_true(is(default_tester(cutoff = 1), "data.table"))
  expect_true(is(default_tester(cutoff = 1000), "data.table"))

})
