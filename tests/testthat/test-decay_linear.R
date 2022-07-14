tester <- function(cutoff = 20) decay_linear(cutoff)

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(c(1, 1)))
  expect_error(tester(-1))
  expect_error(tester(Inf))
  expect_error(tester(NULL))
})

test_that("output is a decay function that returns a numeric", {
  expect_is(tester(), "function")

  value_test <- tester()
  expect_equal(value_test(0), 1)
  expect_equal(value_test(5), 0.75)
  expect_equal(value_test(10), 0.5)
  expect_equal(value_test(20), 0)
  expect_equal(value_test(60), 0)

  value_test <- tester(30)
  expect_equal(value_test(0), 1)
  expect_equal(round(value_test(10), digits = 4), 0.6667)
  expect_equal(value_test(15), 0.5)
  expect_equal(value_test(30), 0)
  expect_equal(value_test(60), 0)
})

test_that("accepts a numeric vector", {
  value_test <- tester()
  expect_equal(value_test(c(0, 5, 10, 20, 60)), c(1, 0.75, 0.5, 0, 0))
})

test_that("returns empty numeric if receives empty integer/integer", {
  value_test <- tester()
  expect_identical(value_test(integer()), numeric())
  expect_identical(value_test(numeric()), numeric())
})
