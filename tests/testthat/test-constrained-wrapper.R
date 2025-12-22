# testthat::local_edition(3)

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


tester <- function(
    constraint = "total",
    travel_matrix = tm,
    land_use_data = lu,
    travel_cost = "travel_time",
    decay_function = decay,
    demand = "population",
    supply = "jobs",
    active = FALSE,
    error_threshold = 0.001,
    improvement_threshold = 1e-6,
    max_iterations = 1000,
    group_by = character(0),
    fill_missing_ids = TRUE,
    detailed_results = FALSE
) {
  constrained_accessibility(
    constraint,
    travel_matrix,
    land_use_data,
    travel_cost,
    decay_function,
    demand,
    supply,
    return_demand_side,
    error_threshold,
    improvement_threshold,
    max_iterations,
    group_by,
    fill_missing_ids,
    detailed_results
  )
}

test_that("constrained_accessibility: wrapper argument rules are enforced", {

  # invalid constraint -> any error is fine


  # total/singly require TRUE or FALSE (not NULL)
  expect_error(
    tester(constraint = "total", return_demand_side = NULL)
  )

  expect_error(
    tester(constraint = "singly", return_demand_side = NULL)
  )

  # doubly requires NULL (error if TRUE)
  expect_error(
    tester(constraint = "doubly", active = FALSE)
  )
})





test_that("raises errors due to incorrect input", {

  expect_error(tester(constraint = "banana"))

  expect_error(tester(decay_function = "a"))
  expect_error(tester(decay_function = mean))
  expect_error(tester(decay_function = get))

  expect_error(tester(opportunity = 1))
  expect_error(tester(opportunity = c("schools", "jobs")))

  expect_error(tester(travel_cost = 1))
  expect_error(tester(travel_cost = c("travel_time", "monetary_cost")))

  expect_error(tester(demand = 1))
  expect_error(tester(demand = c("population", "population")))

  expect_error(tester(supply = 1))
 # expect_error(tester(supply = c("population", "population"))) 666666666

  expect_error(tester(group_by = 1))
  expect_error(tester(group_by = NA))
  expect_error(tester(group_by = "from_id"))
  expect_error(tester(group_by = c("mode", "mode")))

  expect_error(tester(fill_missing_ids = 1))
  expect_error(tester(fill_missing_ids = c(TRUE, TRUE)))
  expect_error(tester(fill_missing_ids = NA))

  expect_error(tester(as.list(tm)))
  expect_error(tester(tm[, .(oi = from_id, to_id, travel_time)]))
  expect_error(tester(tm[, .(from_id, oi = to_id, travel_time)]))
  expect_error(
    tester(
      tm[, .(from_id, to_id, oi = travel_time)],
      travel_cost = "travel_time"
    )
  )
  expect_error(
    tester(
      tm[, .(from_id, to_id, travel_time, oi = mode)],
      group_by = "mode"
    )
  )

  expect_error(tester(as.list(lu)))
  expect_error(
    tester(lu = lu[, .(oi = id, jobs, population)])
  )
  expect_error(
    tester(
      lu = lu[, .(id, oi = jobs, population)],
      opportunity = "jobs"
    )
  )
  expect_error(
    tester(
      lu = lu[, .(id, jobs, oi = population)],
      demand = "population"
    )
  )
})
