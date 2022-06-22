context("Decay binary")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()


default_tester <- function(cutoff = 20) {

  results <- accessibility::decay_binary(cutoff = cutoff)
  return(results)
}


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {


  # input data is not a data.frame
  expect_error( default_tester(cutoff = -1) )
  expect_error( default_tester(cutoff = 'banana') )

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

f_test  <- default_tester(cutoff = 30)

  expect_is(f_test(20), 'numeric')
  expect_equal(f_test(10), 1)
  expect_equal(f_test(60), 0)

})
