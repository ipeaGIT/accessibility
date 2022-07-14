#' Floating catchment area accessibility
#'
#' Calculates accessibility accounting for the competition of resources using a
#' measure from the floating catchment area (FCA) family. Please see the
#' details for the available FCA measures and an explanation on how to use them.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @param fca_metric A string. Which floating catchment area measure to use.
#'   Current available options are `"2sfca"` and `"bfca"`. More info in the
#'   details.
#' @template decay_function
#' @template opportunity_col
#' @template travel_cost_col
#' @param competition_col A string. The name of the column in `land_use_data`
#'   with the number of people in each origin that will be considered potential
#'   competitors. Defaults to `"population"`.
#' @template by_col
#'
#' @template return_accessibility
#'
#' @section Details:
#' The package currently includes two built-in FCA measures:
#'
#' - 2SFCA - the 2-Step Floating Catchment Area measure was the first
#' accessibility metric in the FCA family. It was originally proposed by
#' \insertCite{luo2003measures;textual}{accessibility}.
#'
#' - BFCA - the Balanced Floating Catchment Area measure calculates
#' accessibility accounting for competition effects while simultaneously
#' correcting for issues of inflation of demand and service levels that are
#' present in other FCA measures. It was originally proposed by
#' \insertCite{paez2019demand;textual}{accessibility} and named in
#' \insertCite{pereira2021geographic;textual}{accessibility}.
#'
#' @references
#' \insertAllCited{}
#'
#' @family Floating catchment area
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' # 2SFCA with a step decay function
#' df <- floating_catchment_area(
#'   travel_matrix,
#'   land_use_data,
#'   fca_metric = "2sfca",
#'   decay_function = decay_binary(cutoff = 50),
#'   opportunity_col = "jobs",
#'   travel_cost_col = "travel_time",
#'   competition_col = "population"
#' )
#' head(df)
#'
#'
#' # BFCA with an exponential decay function
#' df <- floating_catchment_area(
#'   travel_matrix,
#'   land_use_data,
#'   fca_metric = "bfca",
#'   decay_function = decay_exponential(decay_value = 0.5),
#'   opportunity_col = "jobs",
#'   travel_cost_col = "travel_time",
#'   competition_col = "population"
#' )
#' head(df)
#'
#' @export
floating_catchment_area <- function(travel_matrix,
                                    land_use_data,
                                    fca_metric,
                                    decay_function,
                                    opportunity_col,
                                    travel_cost_col = "travel_time",
                                    competition_col = "population",
                                    by_col = NULL) {
  by_col_char <- assert_and_assign_by_col(by_col)
  checkmate::assert(
    checkmate::check_string(fca_metric),
    checkmate::check_names(fca_metric, subset.of = c("2sfca", "bfca")),
    combine = "and"
  )
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_string(competition_col)
  assert_decay_function(decay_function)
  assert_travel_matrix(travel_matrix, travel_cost_col, by_col_char)
  assert_land_use_data(
    land_use_data,
    opportunity_col,
    competition = competition_col
  )

  # if not a dt, keep original class to assign later when returning result

  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }

  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  merge_by_reference(data, land_use_data, opportunity_col, active = TRUE)
  merge_by_reference(data, land_use_data, competition_col, active = FALSE)
  data[, opp_weight := decay_function(get(travel_cost_col))]

  warn_extra_cols(
    travel_matrix,
    travel_cost_col, group_id = "from_id",
    groups = c("from_id", by_col_char)
  )

  fca_function <- if (fca_metric == "2sfca") {
    fca_2sfca
  } else {
    fca_bfca
  }

  access <- fca_function(data, opportunity_col, competition_col, by_col_char)

  data.table::setnames(access, c("from_id", "access"), c("id", opportunity_col))

  if (exists("original_class")) class(access) <- original_class

  return(access)
}


#' @keywords internal
fca_2sfca <- function(data, opportunity_col, competition_col, by_col_char) {
  # step 1a - calculate the demand to each destination as the weight between
  # each od pair multiplied by the number of people at the origin

  data[
    ,
    pop_served := sum(get(competition_col) * opp_weight, na.rm = TRUE),
    by = c("to_id", by_col_char)
  ]

  # step 1b - calculate the provider-to-population ratio (ppr) of a destination
  # as number of opportunities/resources located at it divided by the
  # population it serves

  data[, ppr := get(opportunity_col) / pop_served]

  # step 2 - calculate accessibility at each origin i as the sum of the
  # multiplication between the ppr of destination j and the weight between od
  # pair ij

  access <- data[
    ,
    .(access = sum(ppr * opp_weight, na.rm = TRUE)),
    by = c("from_id", by_col_char)
  ]

  return(access)
}


#' @keywords internal
fca_bfca <- function(data, opportunity_col, competition_col, by_col_char) {
  # calculate balanced (normalized) opp_weight by origin and by destination

  data[
    ,
    balanced_opp_weight_i := opp_weight / sum(opp_weight),
    by = c("from_id", by_col_char)
  ]
  data[
    ,
    balanced_opp_weight_j := opp_weight / sum(opp_weight),
    by = c("to_id", by_col_char)
  ]

  # step 1a - calculate the balanced demand to each destination as the balanced
  # weight by origin multiplied by the number of people at the origin

  data[
    ,
    balanced_pop_served := sum(
      get(competition_col) * balanced_opp_weight_i,
      na.rm = TRUE
    ),
    by = c("to_id", by_col_char)
  ]

  # step 1b - calculate the balanced provider-to-population ratio
  # (balanced_ppr) of a destination as number of opportunities/resources
  # located at it divided by the balanced demand

  data[, balanced_ppr := get(opportunity_col) / balanced_pop_served]

  # step 2 - calculate accessibility at each origin i as the sum of the
  # multiplication between the balanced_ppr of destination j and the balanced
  # weight by destination between od pair ij

  access <- data[
    ,
    .(access = sum(balanced_ppr * balanced_opp_weight_j, na.rm = TRUE)),
    by = c("from_id", by_col_char)
  ]

  return(access)
}
