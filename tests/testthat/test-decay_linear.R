context("Decay linear")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()


default_tester <- function(cutoff = 20) {

  results <- accessibility::decay_linear(cutoff = cutoff)
  return(results)
}


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {


  # incorrect input
  expect_error( default_tester(cutoff = -1) )
  expect_error( default_tester(cutoff = 'banana') )
  expect_error( default_tester(cutoff = NULL) )

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

f_test  <- default_tester(cutoff = 30)

  expect_is(f_test(20), 'numeric')
  expect_is(f_test(60), 'numeric')
  expect_equal(round(f_test(10), digits = 4), 0.6667)
  expect_equal(f_test(60), 0)

})
