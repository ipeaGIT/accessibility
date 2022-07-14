tester <- function(decay_value = 0.1) decay_exponential(decay_value)

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
  expect_equal(value_test(10), 1 / exp(1))
  expect_equal(value_test(20), 1 / exp(2))
  expect_equal(value_test(60), 1 / exp(6))

  value_test <- tester(0.05)
  expect_equal(value_test(0), 1)
  expect_equal(value_test(20), 1 / exp(1))
  expect_equal(value_test(40), 1 / exp(2))
  expect_equal(value_test(120), 1 / exp(6))
})

test_that("accepts a numeric vector", {
  value_test <- tester()
  expect_equal(
    value_test(c(0, 10, 20, 60)),
    c(1, 1 / exp(1), 1 / exp(2), 1 / exp(6))
  )
})

test_that("returns empty numeric if receives empty numeric/integer", {
  value_test <- tester()
  expect_identical(value_test(integer()), numeric())
  expect_identical(value_test(numeric()), numeric())
})
