# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

land_use_without_nas <- land_use_data[!is.na(income_decile)]

tester <- function(sociodemographic_data = land_use_without_nas,
                   accessibility_data = small_access,
                   columns = "income_per_capita") {
  assert_sociodemographic_data(
    sociodemographic_data,
    accessibility_data,
    columns
  )
}

test_that("warns if access_data contains ids not in sociodemographic_data", {
  expect_warning(
    tester(
      accessibility_data = rbind(
        small_access,
        data.table::data.table(id = "hehe", mode = "transit", jobs = 100)
      )
    )
  )
})

test_that("warns if cols contain NAs", {
  expect_warning(
    tester(
      rbind(
        land_use_without_nas,
        data.table::data.table(id = "hehe"),
        fill = TRUE
      )
    )
  )
})
