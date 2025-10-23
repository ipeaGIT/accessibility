#' Total constrained
#'
#' Calculates total constrained accessibility, part of a family of accessibility
#'measures proposed in
#'\insertCite{soukhovFamilyAccessibilityMeasures2025;textual}{accessibility} that
#' proportionally allocates the total sum of opportunities in the region based
#' on the relative travel impedance between zones. It is linearly proportion to
#' the conventional gravity-based Hansen-type accessibility measure and units of
#' the resulting values can be understood as the number of opportunities in the
#' region that can be potentially interacted with.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @template decay_function
#' @template group_by
#' @template fill_missing_ids_combinations
#' @param detailed_results A `logical`. Whether to return total constrained
#'   results aggregated by origin-destination pair (`TRUE`) or by origin
#'   (`FALSE`, the default). When `TRUE`, the output also includes the balancing
#'   factor `kappa_tot` used in the calculation and the unconstrained
#'   accessibility value `weighted_opportunity`. Please note that the argument
#'    `fill_missing_ids` does not affect the output when `detailed_results` is
#'    `TRUE`.
#'
#' @template return_accessibility
#'
#' @references
#' \insertAllCited{}
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
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
#' df <- total_constrained(
#'   travel_matrix,
#'   land_use_data,
#'   opportunity = "jobs",
#'   travel_cost = "travel_time",
#'   decay_function = decay_exponential(decay_value = 0.1)
#' )
#' df
#'
#' detailed_df <- total_constrained(
#'   travel_matrix,
#'   land_use_data,
#'   opportunity = "jobs",
#'   travel_cost = "travel_time",
#'   decay_function = decay_exponential(decay_value = 0.1),
#'   detailed_results = TRUE
#' )
#' detailed_df
#'
#' @export
total_constrained <- function(travel_matrix,
                                            land_use_data,
                                            opportunity,
                                            travel_cost,
                                            decay_function,
                                            group_by = character(0),
                                            fill_missing_ids = TRUE,
                                            detailed_results = FALSE) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_logical(detailed_results, len = 1, any.missing = FALSE)
  assert_decay_function(decay_function)
  assert_group_by(group_by)
  assert_detailed_fill_missing_ids(fill_missing_ids, detailed_results)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(
    land_use_data,
    travel_matrix,
    opportunity)

  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }

  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  merge_by_reference(data, land_use_data, opportunity, left_df_idcol = "to_id")

  data <- apply_gravity_measure(data, decay_function, travel_cost)

  groups <- c("from_id", group_by)
  if ("decay_function_arg" %in% names(data)) {
    groups <- c(groups, "decay_function_arg")
    group_by <- c(group_by, "decay_function_arg")
  }

  warn_extra_cols(
    travel_matrix,
    travel_cost,
    group_id = "from_id",
    groups = groups
  )

  # Calculate unconstrained accessibility (weighted_opportunities) per ij: W_j * f(c_ij)
  .opportunity_colname <- opportunity
  data[, weighted_opportunity := get(.opportunity_colname) * opp_weight]

  # Calculate kappa_tot for each ij: kappa_tot_ij = (W_j * f(c_ij)) / sum_over_all_ij (W_j * f(c_ij))
  total_weighted_system <- data[, sum(weighted_opportunity)]
  data[, kappa_tot := weighted_opportunity / total_weighted_system]

  # Total opportunities in the region
  total_opportunities_region <- sum(land_use_data[[.opportunity_colname]])

  # Calculate total constrained opportunity: kappa_tot_ij * total regional opportunities
  data[, constrained_opportunity := kappa_tot * total_opportunities_region]

  if (detailed_results) {
    # Return detailed results with all intermediate calculations
    data[, total_opportunities_region := total_opportunities_region]

    # Drop unnecessary columns and rename
    cols_to_drop <- c(travel_cost, opportunity, "opp_weight", total_opportunities_region)
    access <- data[, (cols_to_drop) := NULL]

    data.table::setnames(access, "constrained_opportunity", opportunity)
  } else {
    # Return aggregated results by origin
    access <- data[
      ,
      .(access = sum(constrained_opportunity)),
      by = c("from_id", group_by)
    ]

    if (fill_missing_ids) {
      access <- fill_missing_ids(access, travel_matrix, groups)
    }

    data.table::setnames(access, c("from_id", "access"), c("id", opportunity))
  }

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}
