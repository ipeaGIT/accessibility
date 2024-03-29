# if running manually, please run the following line first:
# source("tests/testthat/setup.R")

tester <- function(land_use_data = get("land_use_data", envir = parent.frame()),
                   travel_matrix = smaller_matrix,
                   opportunity = "jobs",
                   active = TRUE,
                   demand = NULL) {
  assert_land_use_data(
    land_use_data,
    travel_matrix,
    opportunity,
    active,
    demand
  )
}

test_that("warns if travel_matrix contains ids that land_use_data does not", {
  expect_warning(
    tester(
      travel_matrix = rbind(
        smaller_matrix,
        data.table::data.table(
          mode = "transit",
          from_id = "89a88cdb57bffff",
          to_id = "hehe",
          travel_time = 50
        )
      )
    )
  )

  expect_warning(
    tester(
      travel_matrix = rbind(
        smaller_matrix,
        data.table::data.table(
          mode = "transit",
          from_id = "hehe",
          to_id = "89a88cdb57bffff",
          travel_time = 50
        )
      ),
      active = FALSE
    )
  )

  expect_warning(
    tester(
      travel_matrix = rbind(
        smaller_matrix,
        data.table::data.table(
          mode = "transit",
          from_id = "89a88cdb57bffff",
          to_id = "hehe",
          travel_time = 50
        )
      ),
      active = NULL
    )
  )

  expect_warning(
    tester(
      travel_matrix = rbind(
        smaller_matrix,
        data.table::data.table(
          mode = "transit",
          from_id = "hehe",
          to_id = "89a88cdb57bffff",
          travel_time = 50
        )
      ),
      active = NULL
    )
  )
})

test_that("warns if either opportunity or demand cols contain NAs", {
  expect_warning(
    tester(
      rbind(land_use_data, data.table::data.table(id = "hehe"), fill = TRUE),
      demand = "population"
    )
  )
})

test_that("assert_land_use_data() doesn't error with tibbles", {
  # issue 49 (https://github.com/ipeaGIT/accessibility/issues/49)
  # using land_use_data as tibbles would throw errors because this format has
  # stricter subsetting rules
  expect_no_error(tester(tibble::as_tibble(land_use_data)))
})
