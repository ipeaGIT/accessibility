
testthat::local_edition(3)

test_that("constrained_accessibility: wrapper argument rules are enforced", {
  tm <- data.table::data.table(
    expand.grid(from_id = c("1","2"), to_id = c("1","2"))
  )
  tm[, travel_time := c(10, 20, 15, 10)]
  lu <- data.table::data.table(
    id         = c("1","2"),
    population = c(5, 7),
    jobs       = c(100, 120)
  )
  decay <- decay_exponential(decay_value = 0.1)

  # invalid constraint -> any error is fine
  expect_error(
    constrained_accessibility(
      constraint = "invalid",
      travel_matrix = tm,
      land_use_data = lu,
      travel_cost = "travel_time",
      decay_function = decay,
      demand = "population",
      supply = "jobs",
      return_demand_side = TRUE
    )
  )

  # total/singly require TRUE or FALSE (not NULL)
  expect_error(
    constrained_accessibility(
      constraint = "total",
      travel_matrix = tm,
      land_use_data = lu,
      travel_cost = "travel_time",
      decay_function = decay,
      demand = "population",
      supply = "jobs",
      return_demand_side = NULL
    )
  )
  expect_error(
    constrained_accessibility(
      constraint = "singly",
      travel_matrix = tm,
      land_use_data = lu,
      travel_cost = "travel_time",
      decay_function = decay,
      demand = "population",
      supply = "jobs",
      return_demand_side = NULL
    )
  )

  # doubly requires NULL (error if TRUE)
  expect_error(
    constrained_accessibility(
      constraint = "doubly",
      travel_matrix = tm,
      land_use_data = lu,
      travel_cost = "travel_time",
      decay_function = decay,
      demand = "population",
      supply = "jobs",
      return_demand_side = TRUE
    )
  )
})
