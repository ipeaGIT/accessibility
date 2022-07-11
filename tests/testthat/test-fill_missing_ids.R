test_that("fill correctly with one id column", {
  access_df <- data.table::data.table(
    from_id = c("1", "2", "5"),
    access = c(10, 10, 10)
  )
  possible_combinations <- data.table::data.table(from_id = as.character(1:5))
  groups <- "from_id"

  result <- fill_missing_ids(access_df, possible_combinations, groups)
  data.table::setkey(result, NULL)
  expect_equal(
    result,
    data.table::data.table(
      from_id = as.character(1:5),
      access = c(10, 10, 0, 0, 10)
    )
  )
})

test_that("fill correctly with two id columns", {
  access_df <- data.table::data.table(
    from_id = c("1", "2", "5", "1"),
    mode = c("mode1", "mode1", "mode1", "mode2"),
    access = c(10, 10, 10, 20)
  )
  possible_combinations <- data.table::CJ(
    from_id = as.character(1:5),
    mode = c("mode1", "mode2")
  )
  groups <- c("from_id", "mode")

  result <- fill_missing_ids(access_df, possible_combinations, groups)
  data.table::setkey(result, NULL)
  expect_equal(
    result,
    data.table::data.table(
      from_id = rep(as.character(1:5), each = 2),
      mode = rep(c("mode1", "mode2"), times = 5),
      access = c(10, 20, 10, 0, 0, 0, 0, 0, 10, 0)
    )
  )
})
