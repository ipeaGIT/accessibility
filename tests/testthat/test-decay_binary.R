tester <- function(cutoff = 20) {
  decay_binary(cutoff = cutoff)
}

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(c(1, 1)))
  expect_error(tester(-1))
  expect_error(tester(Inf))
  expect_error(tester(NULL))
})

test_that("output is a decay function that returns an integer", {
  expect_is(tester(), "function")

  value_test <- tester(20)
  expect_equal(value_test(10), 1)
  expect_equal(value_test(20), 1)
  expect_equal(value_test(60), 0)
})
