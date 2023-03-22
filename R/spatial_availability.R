#' Spatial availability
#'
#' Calculates spatial availability, an accessibility measured proposed by
#' \insertCite{soukhov2023introducing;textual}{accessibility} that takes into
#' account competition effects. The accessibility levels that result from using
#' this measure are proportional both to the demand in each origin and to the
#' travel cost it takes to reach the destinations.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @template demand
#' @template decay_function
#' @param alpha A `numeric`. A parameter used to modulate the effect of demand
#'   by population. When less than 1, opportunities are allocated more rapidly
#'   to smaller centers relative to larger ones; values higher than 1 achieve
#'   the opposite effect.
#' @template group_by
#' @template fill_missing_ids_combinations
#'
#' @template return_accessibility
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # the example below is based on Soukhov et al. (2023) paper
#'
#' travel_matrix <- data.table::data.table(
#'   from_id = rep(c("A", "B", "C"), each = 3),
#'   to_id = as.character(rep(1:3, 3)),
#'   travel_time = c(15, 30, 100, 30, 15, 100, 100, 100, 15)
#' )
#' land_use_data <- data.table::data.table(
#'   id = c("A", "B", "C", "1", "2", "3"),
#'   population = c(50000, 150000, 10000, 0, 0, 0),
#'   jobs = c(0, 0, 0, 100000, 100000, 10000)
#' )
#'
#' df <- spatial_availability(
#'   travel_matrix,
#'   land_use_data,
#'   opportunity = "jobs",
#'   travel_cost = "travel_time",
#'   demand = "population",
#'   decay_function = decay_exponential(decay_value = 0.1)
#' )
#' df
#'
#' @export
spatial_availability <- function(travel_matrix,
                                 land_use_data,
                                 opportunity,
                                 travel_cost,
                                 demand,
                                 decay_function,
                                 alpha = 1,
                                 group_by = character(0),
                                 fill_missing_ids = TRUE) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_string(demand)
  checkmate::assert_number(alpha, lower = 0, finite = TRUE)
  checkmate::assert_logical(fill_missing_ids, len = 1, any.missing = FALSE)
  assert_decay_function(decay_function)
  assert_group_by(group_by)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(land_use_data, opportunity, demand)

  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }

  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  merge_by_reference(data, land_use_data, opportunity, active = TRUE)
  merge_by_reference(data, land_use_data, demand, active = FALSE)

  groups <- c("from_id", group_by)
  warn_extra_cols(
    travel_matrix,
    travel_cost,
    group_id = "from_id",
    groups = groups
  )

  .demand_colname <- demand
  total_demand <- sum(land_use_data[[.demand_colname]] ^ alpha)
  data[, demand_bal_fac := (get(.demand_colname) ^ alpha) / total_demand]

  .cost_colname <- travel_cost
  data[, opp_weight := decay_function(get(.cost_colname))]
  data[
    ,
    impedance_bal_fac := opp_weight / sum(opp_weight),
    by = c("to_id", group_by)
  ]

  data[
    ,
    combined_bal_fac := demand_bal_fac * impedance_bal_fac /
      sum(demand_bal_fac * impedance_bal_fac),
    by = c("to_id", group_by)
  ]

  .opportunity_colname <- opportunity
  data[
    ,
    spatial_availability := combined_bal_fac * get(.opportunity_colname),
    by = c("to_id", group_by)
  ]

  access <- data[
    ,
    .(access = sum(spatial_availability)),
    by = c("from_id", group_by)
  ]

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    names(unique_values) <- groups
    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- do_fill_missing_ids(access, possible_combinations, groups)
    }
  }

  data.table::setnames(access, c("from_id", "access"), c("id", opportunity))

  if (exists("original_class")) class(access) <- original_class

  return(access)
}