context("Impedance functions")

# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()




default_tester <- function(t_ij = 30,
                           decay_function = 'linear',
                           cutoff = 60,
                           decay_value=0.5) {

  imp_factor <- accessibility::impedance_fun(t_ij = t_ij,
                                    decay_function = decay_function,
                                    cutoff = cutoff,
                                    decay_value = decay_value)
  return(imp_factor)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # invalid t_ij
  expect_error(default_tester(t_ij = -1))
  expect_error(default_tester(t_ij = Inf))
  expect_error(default_tester(t_ij = 'banana'))
  expect_error(default_tester(t_ij = NULL))

  # invalid decay_function
  expect_error(default_tester(decay_function = -1))
  expect_error(default_tester(decay_function = Inf))
  expect_error(default_tester(decay_function = 'banana'))
  expect_error(default_tester(decay_function = NULL))

  # invalid cutoff
  expect_error(default_tester(cutoff = -1))
  expect_error(default_tester(cutoff = 'banana'))

  # invalid decay_value
  expect_error(default_tester(decay_value = -1))
  expect_error(default_tester(decay_value = Inf))
  expect_error(default_tester(decay_value = 'banana'))

  # invalid combinations of decay_function and decay_value
  expect_error( default_tester(decay_function = 'negative_exponential', decay_value = NULL) )
  expect_error( default_tester(decay_function = 'inverse_power', decay_value = NULL) )
  expect_error( default_tester(decay_function = 'modified_gaussian', decay_value = NULL) )
  expect_error( default_tester(decay_function = 'step', cutoff = NULL) )
  expect_error( default_tester(decay_function = 'linear', cutoff = NULL) )

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values


  # output should be numeric
  expect_is( default_tester(decay_function = 'linear'), 'numeric' )
  expect_is( default_tester(decay_function = 'step'), 'numeric' )
  expect_is( default_tester(decay_function = 'negative_exponential'), 'numeric' )
  expect_is( default_tester(decay_function = 'inverse_power'), 'numeric' )
  expect_is( default_tester(decay_function = 'modified_gaussian'), 'numeric' )

})
