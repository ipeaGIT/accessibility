tester <- function(cutoff = 20) decay_binary(cutoff)

test_that("adequately raises errors", {
  expect_error(tester("banana"))
  expect_error(tester(-1))
  expect_error(tester(Inf))
  expect_error(tester(NA))
  expect_error(tester(numeric(0)))
  expect_error(tester(c(1, 1)))
})

test_that("output is a decay function that returns a numeric", {
  expect_is(tester(), "function")

  output_fn <- tester()
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "20")

  expect_equal(
    output_fn(c(10, 20, 40, 60))[["20"]],
    c(1, 1, 0, 0)
  )

  output_fn <- tester(40)
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 1)
  expect_named(output_list, "40")

  expect_equal(
    output_fn(c(10, 20, 40, 60))[["40"]],
    c(1, 1, 1, 0)
  )

  output_fn <- tester(c(20, 40))
  output_list <- output_fn(0)
  expect_is(output_list, "list")
  expect_length(output_list, 2)
  expect_named(output_list, c("20", "40"))

  expect_equal(
    output_fn(c(10, 20, 40, 60))[["20"]],
    c(1, 1, 0, 0)
  )
  expect_equal(
    output_fn(c(10, 20, 40, 60))[["40"]],
    c(1, 1, 1, 0)
  )
})

test_that("output fn returns empty integer if receives empty numeric/integer", {
  output_fn <- tester(c(20, 40))

  expect_identical(output_fn(integer())[["20"]], integer())
  expect_identical(output_fn(integer())[["40"]], integer())

  expect_identical(output_fn(numeric())[["20"]], integer())
  expect_identical(output_fn(numeric())[["40"]], integer())
})
