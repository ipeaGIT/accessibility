tester <- function(decay_value = 0.5) decay_power(decay_value)

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(-0.1))
  expect_error(tester(Inf))
  expect_error(tester(NA))
  expect_error(tester(numeric(0)))
  expect_error(tester(c(0.1, 0.1)))
})

test_that("output is a decay function that returns a numeric", {
  expect_is(tester(), "function")

  output_fn <- tester()
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "0.5")

  expect_equal(
    output_fn(c(0, 0.5, 1, 16, 64))[["0.5"]],
    c(1, 1, 1, 0.25, 0.125)
  )

  output_fn <- tester(0.25)
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "0.25")

  expect_equal(
    output_fn(c(0, 0.5, 1, 16, 256))[["0.25"]],
    c(1, 1, 1, 0.5, 0.25)
  )

  output_fn <- tester(c(0.5, 0.25))
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 2)
  expect_named(output_list, c("0.5", "0.25"))

  expect_equal(
    output_fn(c(0, 0.5, 1, 16, 64))[["0.5"]],
    c(1, 1, 1, 0.25, 0.125)
  )
  expect_equal(
    output_fn(c(0, 0.5, 1, 16, 256))[["0.25"]],
    c(1, 1, 1, 0.5, 0.25)
  )
})

test_that("output fn returns empty numeric if receives empty numeric/integer", {
  output_fn <- tester(c(0.5, 0.25))

  expect_identical(output_fn(integer())[["0.5"]], numeric())
  expect_identical(output_fn(integer())[["0.25"]], numeric())

  expect_identical(output_fn(numeric())[["0.5"]], numeric())
  expect_identical(output_fn(numeric())[["0.25"]], numeric())
})
