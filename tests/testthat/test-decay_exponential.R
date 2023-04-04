tester <- function(decay_value = 0.1) decay_exponential(decay_value)

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(-0.1))
  expect_error(tester(Inf))
  expect_error(tester(NA))
  expect_error(tester(numeric(0)))
  expect_error(tester(c(0.1, 0.1)))
})

test_that("output is a decay function that returns a list of numeric vctrs", {
  expect_is(tester(), "function")

  output_fn <- tester()
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "0.1")

  expect_equal(
    output_fn(c(0, 10, 20, 60))[["0.1"]],
    c(1, 1 / exp(1), 1 / exp(2), 1 / exp(6))
  )

  output_fn <- tester(0.05)
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "0.05")

  expect_equal(
    output_fn(c(0, 20, 40, 120))[["0.05"]],
    c(1, 1 / exp(1), 1 / exp(2), 1 / exp(6))
  )

  output_fn <- tester(c(0.05, 0.1))
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 2)
  expect_named(output_list, c("0.05", "0.1"))

  expect_equal(
    output_fn(c(0, 10, 20, 60))[["0.1"]],
    c(1, 1 / exp(1), 1 / exp(2), 1 / exp(6))
  )
  expect_equal(
    output_fn(c(0, 20, 40, 120))[["0.05"]],
    c(1, 1 / exp(1), 1 / exp(2), 1 / exp(6))
  )
})

test_that("output fn returns empty numeric if receives empty numeric/integer", {
  output_fn <- tester(c(0.05, 0.1))

  expect_identical(output_fn(integer())[["0.05"]], numeric())
  expect_identical(output_fn(integer())[["0.1"]], numeric())

  expect_identical(output_fn(numeric())[["0.05"]], numeric())
  expect_identical(output_fn(numeric())[["0.1"]], numeric())
})
