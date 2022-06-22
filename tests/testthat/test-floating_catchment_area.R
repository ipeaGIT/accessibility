context("Floating catchment area metrics")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()

default_tester <- function(data = ttm,
                           fca_metric = '2SFCA',
                           orig_col = 'from_id',
                           dest_col = 'to_id',
                           opportunity_col = 'jobs',
                           population_col = 'population',
                           decay_function = decay_linear(cutoff = 50)) {

  results <- accessibility::floating_catchment_area(data = data,
                           fca_metric=fca_metric,
                           orig_col <- orig_col,
                           dest_col <- dest_col,
                           opportunity_col <- opportunity_col,
                           population_col <- population_col,
                           decay_function= decay_function
                           )
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # orig_col and dest_col do not exist in data input
  expect_error(default_tester(orig_col = 'banana'))
  expect_error(default_tester(dest_col = 'banana'))
  expect_error(default_tester(orig_col = 999))
  expect_error(default_tester(dest_col = 999))

  # opportunity_col and population_col do not exist in data input
  expect_error(default_tester(opportunity_col = 'banana'))
  expect_error(default_tester(population_col = 'banana'))
  expect_error(default_tester(opportunity_col = 999))
  expect_error(default_tester(population_col = 999))

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values

  # different fca_metric
  expect_is( default_tester(fca_metric = 'BFCA'), "data.table")

  # different opportunity_colname
  expect_is( default_tester(opportunity_col = 'schools'), "data.table")

  # different decay function
  expect_is( default_tester(decay_function= decay_linear(cutoff = 50)), "data.table")
  expect_is( default_tester(decay_function= decay_exponential(decay_value = 0.5)), "data.table")

})