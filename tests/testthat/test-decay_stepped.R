tester <- function(steps = c(25, 50), values = c(0.5, 0)) {
  decay_stepped(steps, values)
}

test_that("adequately raises errors", {
  expect_error(tester("a"))
  expect_error(tester(integer()))
  expect_error(tester(0))
  expect_error(tester(Inf))
  expect_error(tester(NA))
  expect_error(tester(c(5, 2)))
  expect_error(tester(c(5, 5)))

  expect_error(tester(values = "a"))
  expect_error(tester(c(20, 50), values = 1))
  expect_error(tester(c(20, 50), values = c(1.5, 0)))
  expect_error(tester(c(20, 50), values = c(0.7, -1)))
  expect_error(tester(c(20, 50), values = c(0.7, NA)))
})

test_that("output is a function that returns a numeric vector", {
  expect_is(tester(), "function")

  value_test <- tester()
  expect_identical(value_test(c(5, 30, 55)), c(1, 0.5, 0))

  value_test <- tester(
    c(10, 20, 30, 40),
    c(0.75, 0.5, 0.25, 0)
  )
  expect_identical(
    value_test(c(5, 15, 25, 35, 45)),
    c(1, 0.75, 0.5, 0.25, 0)
  )
})

test_that("steps intervals are open on the right", {
  value_test <- tester(
    c(10, 20, 30, 40),
    c(0.75, 0.5, 0.25, 0)
  )
  expect_identical(
    value_test(c(0, 10, 20, 30, 40)),
    c(1, 0.75, 0.5, 0.25, 0)
  )
})

test_that("returns empty numeric if receives empty numeric/integer", {
  value_test <- tester()
  expect_identical(value_test(integer()), numeric())
  expect_identical(value_test(numeric()), numeric())
})
