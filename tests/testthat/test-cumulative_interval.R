# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

testthat::skip_on_cran()
testthat::skip("skipping for now")

default_tester <- function(data = ttm,
                           interval = c(20, 25),
                           stat = 'mean',
                           opportunity_col = 'schools',
                           travel_cost_col = 'travel_time',
                           by_col='from_id') {

  results <- accessibility::cumulative_time_interval(data = data,
                                      interval = interval,
                                      stat = stat,
                                      opportunity_col = opportunity_col,
                                      travel_cost_col = travel_cost_col,
                                      by_col = by_col
                                      )
  return(results)
  }


# errors and warnings -----------------------------------------------------


test_that("adequately raises errors", {

  # input data is not a data.frame
  expect_error(default_tester(data = list(ttm)))

  # vars with col names do not exist in data input
  expect_error(default_tester(opportunity_col = 'banana'))
  expect_error(default_tester(by_col = 'banana'))
  expect_error(default_tester(travel_cost_col = 'banana'))
  expect_error(default_tester(opportunity_col = 999))
  expect_error(default_tester(by_col = 999))
  expect_error(default_tester(travel_cost_col = 999))

  # invalid summary stat
  expect_error(default_tester(stat = 'banana'))
  expect_error(default_tester(stat = 999))


  # interval values are not positive numeric
  expect_error(default_tester(interval = c(1,2,3)))
  expect_error(default_tester(interval = c(5,5)))
  expect_error(default_tester(interval = c(1,-2)))
  expect_error(default_tester(interval = c(1,Inf)))
  expect_error(default_tester(interval = "banana"))
  expect_error(default_tester(interval = -3))
  expect_error(is(default_tester(interval = Inf), "data.table"))

})



# adequate behavior ------------------------------------------------------


test_that("output is correct", {

  # TO DO:
  #> test output values


  # different opportunity_col
  expect_is( default_tester(opportunity_col = 'jobs'), "data.table")

  # different by_col
  expect_is( default_tester(by_col = 'from_id'), "data.table")

  # different interval values
  expect_is( default_tester(interval = c(10, 15)), "data.table")
  expect_is( default_tester(interval = c(0, 15)), "data.table")
  expect_is( default_tester(interval = c(20, 10)), "data.table")

  # different summary stat
  expect_is( default_tester(stat = 'median'), "data.table")

})
