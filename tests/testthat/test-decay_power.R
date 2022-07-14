tester <- function(decay_value = 0.5) decay_power(decay_value)

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(c(0.1, 0.1)))
  expect_error(tester(-0.1))
  expect_error(tester(Inf))
  expect_error(tester(NULL))
})

test_that("output is a decay function that returns a numeric", {
  expect_is(tester(), "function")

  value_test <- tester()
  expect_equal(value_test(0), 1)
  expect_equal(value_test(0.5), 1)
  expect_equal(value_test(1), 1)
  expect_equal(value_test(16), 0.25)
  expect_equal(value_test(64), 0.125)

  value_test <- tester(0.25)
  expect_equal(value_test(0), 1)
  expect_equal(value_test(0), 1)
  expect_equal(value_test(0.5), 1)
  expect_equal(value_test(1), 1)
  expect_equal(value_test(16), 0.5)
  expect_equal(value_test(256), 0.25)
})

test_that("accepts a numeric vector", {
  value_test <- tester()
  expect_equal(value_test(c(0, 0.5, 1, 16, 64)), c(1, 1, 1, 0.25, 0.125))
})

test_that("returns empty numeric if receives empty numeric/integer", {
  value_test <- tester()
  expect_identical(value_test(integer()), numeric())
  expect_identical(value_test(numeric()), numeric())
})
