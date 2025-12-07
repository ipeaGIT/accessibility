#' Doubly constrained accessibility
#'
#' Calculates accessibility using Wilson's doubly-constrained gravity model.
#' This measure allocates flows between origins and destinations such that
#' origin totals equal demand and destination totals equal supply. This is an
#' internal helper function used by [constrained_accessibility()] when
#' `constraint = "doubly"`.
#'
#' @name doubly_constrained
#' @keywords internal
#' @return A `data.table`/`data.frame` with either OD-level flows (`detailed_results = TRUE`)
#'   or marginals; see Details.
#' @examples NULL
#' @importFrom utils globalVariables
utils::globalVariables(c(
  # common ids / columns
  "from_id", "to_id", "id", "opp_weight", "supply", "demand", "weighted_demand", "weighted_supply",
  # total-constrained
  "kappa_total", "hatkappa_total", "K_total", "hatK_total", "constrained_opportunity",
  # singly-constrained
  "denom_i", "denom_j", "kappa_singly", "hatkappa_singly", "A_i", "B_j", "singly_access",
  # doubly-constrained
  "kappa_doubly", "flow", "error"
))
doubly_constrained <- function(travel_matrix,
                               land_use_data,
                               travel_cost,
                               decay_function,
                               demand,
                               supply,
                               return_demand_side = NULL,
                               error_threshold = 0.001,
                               improvement_threshold = 1e-6,
                               max_iterations = 1000,
                               group_by = character(0),
                               fill_missing_ids = TRUE,
                               detailed_results = FALSE) {
  # Validate inputs
  checkmate::assert_string(travel_cost)
  checkmate::assert_string(demand)
  checkmate::assert_string(supply)
  checkmate::assert_null(return_demand_side)
  checkmate::assert_number(error_threshold, lower = 0)
  checkmate::assert_number(improvement_threshold, lower = 0)
  checkmate::assert_int(max_iterations, lower = 1)
  checkmate::assert_logical(detailed_results, len = 1)
  assert_decay_function(decay_function)
  assert_group_by(group_by)
  assert_detailed_fill_missing_ids(fill_missing_ids, detailed_results)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(land_use_data, travel_matrix, opportunity = supply, demand = demand)

  # Convert to data.table
  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }
  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  # Merge demand and supply
  merge_by_reference(data, land_use_data, demand, left_df_idcol = "from_id")
  merge_by_reference(data, land_use_data, supply, left_df_idcol = "to_id")

  # Apply decay
  data <- apply_gravity_measure(data, decay_function, travel_cost)

  # Prepare vectors and impedance matrix
  origins <- unique(data$from_id)
  destinations <- unique(data$to_id)
  O <- land_use_data[id %in% origins, get(demand)]
  D <- land_use_data[id %in% destinations, get(supply)]

  # Validate totals match
  if (abs(sum(O) - sum(D)) > 1e-6) {
    stop("For doubly-constrained, the sum of origins must equal the sum of destinations.")
  }

  # Build impedance matrix
  impedance_matrix <- matrix(data$opp_weight, nrow = length(origins), ncol = length(destinations), byrow = FALSE)

  # Iterative proportional fitting
  Ai <- rep(1, length(O))
  Bj <- rep(1, length(D))
  previous_error <- Inf
  iteration_count <- 0
  stop_reason <- ""

  repeat {
    iteration_count <- iteration_count + 1

    # Update Ai
    for (i in seq_along(O)) {
      Ai[i] <- 1 / (sum(Bj * D * impedance_matrix[i, ]) + 1e-9)
    }

    # Update Bj
    Bj_new <- numeric(length(D))
    for (j in seq_along(D)) {
      Bj_new[j] <- 1 / (sum(Ai * O * impedance_matrix[, j]) + 1e-9)
    }

    # Compute flows
    Tij <- outer(Ai * O, Bj_new * D) * impedance_matrix

    # Compute error
    error <- (sum(abs(O - rowSums(Tij))) + sum(abs(D - colSums(Tij)))) / sum(O)
    error_change <- abs(previous_error - error)

    if (error < error_threshold || error_change < improvement_threshold || iteration_count >= max_iterations) {
      Bj <- Bj_new
      stop_reason <- if (iteration_count >= max_iterations) "Max iterations reached" else if (error < error_threshold) "Error threshold met" else "Slow improvement"
      break
    }

    previous_error <- error
    Bj <- Bj_new
  }

  # Compute kappa_doubly
# kappa_ij^D = A_i * B_j * O_i * f(c_ij)
  kappa_matrix <- outer(Ai * O, Bj) * impedance_matrix

  # Prepare output
  detailed_dt <- data.table::data.table(
    from_id = rep(origins, each = length(destinations)),
    to_id = rep(destinations, times = length(origins)),
    flow = as.vector(t(Tij)),
    A_i = rep(Ai, each = length(destinations)),
    B_j = rep(Bj, times = length(origins)),
    kappa_doubly = as.vector(t(kappa_matrix)),
    error = error
  )

  if (detailed_results) {
    return(detailed_dt[])
  } else {
    warning("Aggregated results equal marginals (O_i or D_j). Interpret with caution.")
    agg <- data.table::rbindlist(list(
      data.table::data.table(id = origins,      flow = O),
      data.table::data.table(id = destinations, flow = D)
    ))
    return(agg[])
  }

}
