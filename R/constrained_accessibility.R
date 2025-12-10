#' Constrained accessibility
#'
#' Calculates accessibility using constrained gravity models, as proposed in
#' \insertCite{soukhov2025family;textual}{accessibility}:
#' - `"total"`: Allocates total opportunities proportionally based on travel impedance.
#' - `"singly"`: Allocates opportunities proportionally, constrained on one side (demand or supply).
#' - `"doubly"`: Allocates flows so origin totals equal demand and destination totals equal supply.
#' Please see the Details section for more information.
#'
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template travel_cost
#' @template decay_function
#' @template demand
#' @template supply
#' @param constraint A string. One of `"total"`, `"singly"`, or `"doubly"`. See
#'        Details section for more information.
#' @param active A logical. When `TRUE`, the function calculates active
#'        accessibility (the quantity of opportunities that can be reached from
#'        a given origin). when `FALSE`, it calculates passive accessibility (by
#'        how many people each destination can be reached), which is equivalent
#'        to the notion of market potential. This parameter only works for
#'        `constraint` types `"total"` and `"singly"`. Ignored for
#'        `constraint = "doubly"`.
#' @param error_threshold Numeric. Convergence criterion used only for
#'        doubly-constrained case.
#' @param improvement_threshold Numeric. Convergence criterion for improvement
#'        used only for doubly-constrained case.
#' @param max_iterations Integer. Maximum iterations used only for
#'        doubly-constrained calibration.
#' @template group_by
#' @template fill_missing_ids_combinations
#' @param detailed_results Logical. Whether to return detailed OD-level results.
#'
#' @section Details:
#' This function covers the family of constrained accessibility measures
#' proposed in \insertCite{soukhov2025family;textual}{accessibility}.
#'
#' ## Total constrained accessibility
#'
#' Sum of accessibility equals total opportunities (supply) in the region.
#' It allocates total opportunities in the region proportionally based on travel
#' impedance. Uses the logic of a total ~(or unconstrained by Wilson's terms)~
#' constraint. Returns values as either `demand` or `supply`. When
#' `active = FALSE` (market potential variant) is also available.
#'
#' ## Singly constrained accessibility
#'
#' Allocates opportunities at each destination proportionally based on travel
#' impedance and population at the origin. Uses the logic of single constraint
#' from \insertCite{wilson1971family;textual}{accessibility}. Returns values as
#' either 'demand' or 'supply'. Supply-constrained (destination totals fixed)
#' when `market_potential = FALSE`. In either case, totals match either the
#' demand at each origin or supply at each destination, depending on variant.
#' This is equivalent to the `spatial_availability()` function.
#'
#'
#' ## Doubly constrained accessibility
#'
#' Calculates accessibility using doubly-constrained gravity model of
#' \insertCite{wilson1971family;textual}{accessibility}. This measure allocates
#' flows between origins and destinations such that origin totals equal demand
#' and destination totals equal supply. Iterative proportional fitting updates
#' (A_i) and (B_j) until convergence. This ensures that row sums equal (O_i)
#' (demand) and column sums equal (D_j) (supply). Note, only OD-level outputs
#' are available (as aggregate outputs just match inputs).
#'
#' @references
#' \insertAllCited{}

#' @family Constrained accessibility
#'
#' @examples
#' # Load demo data shipped with the package
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' # Total-constrained (supply-side)
#' constrained_accessibility(
#'   constraint =   "total",
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = NULL,
#'   supply          = "jobs",
#'   active = FALSE
#' )
#'
#' # Singly-constrained (demand-side)
#' constrained_accessibility(
#'   constraint =   "singly",
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = "population",
#'   supply          = "jobs",
#'   active = TRUE
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
#' constrained_accessibility(
#'   constraint = "doubly",
#'   travel_matrix = tm_small,
#'   land_use_data = lu_small,
#'   travel_cost     = "travel_time",
#'   decay_function  = decay_exponential(0.1),
#'   demand          = "population",
#'   supply          = "jobs"
#' )
#'
#' @export
constrained_accessibility <- function(constraint,
                                      travel_matrix,
                                      land_use_data,
                                      travel_cost,
                                      decay_function,
                                      demand = NULL,
                                      supply = NULL,
                                      active = TRUE,
                                      error_threshold = 0.001,
                                      improvement_threshold = 1e-6,
                                      max_iterations = 1000,
                                      group_by = character(0),
                                      fill_missing_ids = TRUE,
                                      detailed_results = FALSE) {

  checkmate::assert_choice(constraint, c("total", "singly", "doubly"))
  checkmate::assert_string(travel_cost)
  assert_decay_function(decay_function)
  assert_group_by(group_by)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(land_use_data, travel_matrix, opportunity = supply, demand = demand)
  checkmate::assert_logical(detailed_results, len = 1, any.missing = FALSE)
  assert_detailed_fill_missing_ids(fill_missing_ids, detailed_results)


  if (constraint == "doubly") {
    if (!is.null(active)) {
      stop("For 'doubly', the 'active' parameter is not used.")
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

  if (is.null(active)) {
    stop(sprintf("For '%s', active must be TRUE or FALSE.", constraint))
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
      active = active,
      demand = if (!active) demand else NULL,
      supply = if (active) supply else NULL
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
      active = active,
      group_by = group_by,
      fill_missing_ids = fill_missing_ids,
      detailed_results = detailed_results
    ))
  }
}
