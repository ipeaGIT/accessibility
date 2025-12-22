# Small internal helpers
make_tm_3x3 <- function() {
  tm <- data.table::data.table(
    expand.grid(from_id = c("1","2","3"), to_id = c("1","2","3"))
  )
  tm[, travel_time := c(10, 30, 15,  30, 10, 25,  15, 25, 10)]
  tm
}
make_lu_large <- function() {
  data.table::data.table(
    id         = c("1","2","3"),
    population = c(4, 10, 6),
    jobs       = c(160, 150, 180)
  )
}
make_lu_match20 <- function() {
  data.table::data.table(
    id         = c("1","2","3"),
    population = c(4, 10, 6),  # sum = 20
    jobs       = c(7,  5,  8)  # sum = 20
  )
}

exp_decay_01 <- decay_exponential(decay_value = 0.1)
pow_decay_3  <- decay_power(decay_value = 3)

# ------------------------------- TOTAL -------------------------------
test_that("total-constrained: preserves totals on supply/demand sides", {
  tm <- make_tm_3x3()
  lu <- make_lu_large()

  # Supply-side output (returns accessible opportunities)
  det_supply <- constrained_accessibility("total", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = NULL, supply = "jobs",
                                          detailed_results = TRUE, active = TRUE
  )
  agg_supply <- constrained_accessibility("total", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = NULL, supply = "jobs",
                                          detailed_results = FALSE, active = TRUE
  )

  # Demand-side output (returns accessible population)
  det_demand <- constrained_accessibility("total", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = NULL,
                                          detailed_results = TRUE, active = FALSE
  )
  agg_demand <- constrained_accessibility("total", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = NULL,
                                          detailed_results = FALSE, active = FALSE
  )

  expect_equal(sum(det_supply$supply), sum(lu$jobs))
  expect_equal(sum(agg_supply$supply), sum(lu$jobs))
  expect_equal(sum(det_demand$demand), sum(lu$population))
  expect_equal(sum(agg_demand$demand), sum(lu$population))
})

# ------------------------------ SINGLY -------------------------------
test_that("singly-constrained: preserves totals and kappa properties", {
  tm <- make_tm_3x3()
  lu <- make_lu_large()

  # Supply-constrained (returns accessible opportunities)
  det_supply <- constrained_accessibility("singly", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = "jobs",
                                          detailed_results = TRUE, active = TRUE
  )
  agg_supply <- constrained_accessibility("singly", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = "jobs",
                                          detailed_results = FALSE, active = TRUE
  )

  # Demand-constrained (returns accessible population)
  det_demand <- constrained_accessibility("singly", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = "jobs",
                                          detailed_results = TRUE, active = FALSE
  )
  agg_demand <- constrained_accessibility("singly", tm, lu,
                                          travel_cost = "travel_time", decay_function = exp_decay_01,
                                          demand = "population", supply = "jobs",
                                          detailed_results = FALSE, active = FALSE
  )

  # Totals
  expect_equal(sum(det_supply$supply), sum(lu$jobs))
  expect_equal(sum(agg_supply$supply), sum(lu$jobs))
  expect_equal(sum(det_demand$demand), sum(lu$population))
  expect_equal(sum(agg_demand$demand), sum(lu$population))

  # κ normalization properties (these DO hold for singly)
  by_to   <- det_supply[, .(sum_k = sum(kappa_singly)),    by = to_id]
  by_from <- det_demand[, .(sum_k = sum(hatkappa_singly)), by = from_id]
  expect_true(all(abs(by_to$sum_k   - 1) < 1e-8))  # per-destination sums == 1
  expect_true(all(abs(by_from$sum_k - 1) < 1e-8))  # per-origin sums == 1
  expect_equal(sum(det_supply$kappa_singly),    length(unique(det_supply$to_id)))
  expect_equal(sum(det_demand$hatkappa_singly), length(unique(det_demand$from_id)))
})

# ------------------------------ DOUBLY -------------------------------
test_that("doubly-constrained: errors when totals mismatch", {
  tm <- make_tm_3x3()
  lu_bad <- data.table::copy(make_lu_match20())
  lu_bad[id == "3", jobs := jobs + 1]  # break equality

  expect_error(
    constrained_accessibility("doubly", tm, lu_bad,
                              travel_cost = "travel_time", decay_function = exp_decay_01,
                              demand = "population", supply = "jobs",
                              detailed_results = TRUE, active = NULL
    ),
    "sum of origins must equal the sum of destinations"
  )
})



test_that("doubly-constrained: OD flows match marginals", {
  tm    <- make_tm_3x3()
  lu    <- make_lu_match20()
  decay <- pow_decay_3

  det <- constrained_accessibility("doubly", tm, lu,
                                   travel_cost = "travel_time", decay_function = decay,
                                   demand = "population", supply = "jobs",
                                   detailed_results = TRUE, active = NULL
  )

  # Robustly pick the numeric flow column
  candidate_cols <- c("flow", "supply", "demand")
  value_col <- intersect(candidate_cols, names(det))[1]
  expect_true(!is.null(value_col), info = "No flow/supply/demand column in detailed output")
  expect_true(is.numeric(det[[value_col]]), info = "Flow column must be numeric")

  # Sum flows by origin and destination
  by_origin <- det[, .(sum_flow = sum(get(value_col))), by = from_id]
  by_dest   <- det[, .(sum_flow = sum(get(value_col))), by = to_id]

  # Build expected marginals from land use
  O <- lu[match(by_origin$from_id, lu$id), population]
  D <- lu[match(by_dest$to_id,   lu$id), jobs]

  expect_equal(by_origin$sum_flow, O, tolerance = 1e-3)
  expect_equal(by_dest$sum_flow,   D, tolerance = 1e-3)
})
