#' Constrained accessibility
#'
#' Calculates accessibility using constrained gravity models:
#' - `"total"`: Allocates total opportunities proportionally based on travel impedance.
#' - `"singly"`: Allocates opportunities proportionally, constrained on one side (demand or supply).
#' - `"doubly"`: Allocates flows so origin totals equal demand and destination totals equal supply.
#'
#' @template description_generic_cost
#' @template travel_matrix
#' @template land_use_data
#' @template travel_cost
#' @template decay_function
#' @template demand
#' @template supply
#' @param constraint A string. One of `"total"`, `"singly"`, or `"doubly"`.
#' @param return_demand_side Logical for `"total"` and `"singly"`, must be `NULL` for `"doubly"`.
#' @param error_threshold Numeric. Convergence criterion for doubly-constrained case.
#' @param improvement_threshold Numeric. Convergence criterion for improvement.
#' @param max_iterations Integer. Maximum iterations for doubly-constrained calibration.
#' @template group_by
#' @template fill_missing_ids_combinations
#' @param detailed_results Logical. Whether to return detailed OD-level results.
#'
#' @details
#' See individual function documentation for mathematical details:
#' [total_constrained()], [singly_constrained()], [doubly_constrained()].
#'
#' @family Constrained accessibility
#'
#' @examples
#' # Load demo data shipped with the package (used for 'total' and 'singly')
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' # Total-constrained (supply-side)
#' constrained_accessibility("total", travel_matrix, land_use_data,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = NULL,
#'   supply          = "jobs",
#'   return_demand_side = FALSE
#' )
#'
#' # Singly-constrained (demand-side)
#' constrained_accessibility("singly", travel_matrix, land_use_data,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = "population",
#'   supply          = "jobs",
#'   return_demand_side = TRUE
#' )
#'
#' # Doubly-constrained: use a small toy dataset with matching totals
#' tm_small <- data.table::data.table(
#'   expand.grid(from_id = c("1","2","3"), to_id = c("1","2","3"))
#' )
#' tm_small[, travel_time := c(10, 30, 15, 30, 10, 25, 15, 25, 10)]
#' lu_small <- data.table::data.table(
#'   id         = c("1","2","3"),
#'   population = c(4, 10, 6),   # sum = 20
#'   jobs       = c(7,  5,  8)   # sum = 20
#' )
#'
#' constrained_accessibility("doubly", tm_small, lu_small,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = "population",
#'   supply          = "jobs",
#'   return_demand_side = NULL
#' )
#' @export
constrained_accessibility <- function(constraint,
                                      travel_matrix,
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

  checkmate::assert_choice(constraint, c("total", "singly", "doubly"))


  if (constraint == "doubly") {
    if (!is.null(return_demand_side)) {
      stop("For 'doubly', return_demand_side must be NULL.")
    }
    return(doubly_constrained(
      travel_matrix = travel_matrix,
      land_use_data = land_use_data,
      travel_cost = travel_cost,
      decay_function = decay_function,
      demand = demand,
      supply = supply,
      error_threshold = error_threshold,
      improvement_threshold = improvement_threshold,
      max_iterations = max_iterations,
      group_by = group_by,
      fill_missing_ids = fill_missing_ids,
      detailed_results = detailed_results
    ))
  }

  if (is.null(return_demand_side)) {
    stop(sprintf("For '%s', return_demand_side must be TRUE or FALSE.", constraint))
  }

  if (constraint == "total") {
    return(total_constrained(
      travel_matrix = travel_matrix,
      land_use_data = land_use_data,
      travel_cost = travel_cost,
      decay_function = decay_function,
      group_by = group_by,
      fill_missing_ids = fill_missing_ids,
      detailed_results = detailed_results,
      return_demand_side = return_demand_side,
      demand = if (return_demand_side) demand else NULL,
      supply = if (!return_demand_side) supply else NULL
    ))
  }

  if (constraint == "singly") {
    return(singly_constrained(
      travel_matrix = travel_matrix,
      land_use_data = land_use_data,
      travel_cost = travel_cost,
      decay_function = decay_function,
      demand = demand,
      supply = supply,
      return_demand_side = return_demand_side,
      group_by = group_by,
      fill_missing_ids = fill_missing_ids,
      detailed_results = detailed_results
    ))
  }
}
