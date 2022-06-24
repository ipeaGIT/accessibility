#' @title Gravity-based accessibility measures
#'
#' @description
#' The function calculates gravity-based accessibility measures. Accessibility
#' can be calculated using multiple alternative decay functions passed through
#' the `decay_function` parameter.
#'
#' @template input_data
#' @template opportunity_col
#' @template decay_function
#' @template travel_cost_col
#' @template by_col
#'
#' @return A `data.table` object.
#' @family Gravity-based accessibility
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df_linear <- gravity_access(data = ttm,
#'                            opportunity_col = 'schools',
#'                            decay_function = decay_linear(cutoff = 50),
#'                            travel_cost_col = 'travel_time',
#'                            by_col = 'from_id'
#'                            )
#'
#'head(df_linear)
#'
#'df_exp <- gravity_access(data = ttm,
#'                         opportunity_col = 'schools',
#'                         decay_function = decay_exponential(decay_value = 0.5),
#'                         travel_cost_col = 'travel_time',
#'                         by_col = 'from_id'
#'                         )
#'head(df_exp)
#'
#' @export
gravity_access <- function(data,
                           opportunity_col,
                           decay_function,
                           travel_cost_col = 'travel_time',
                           by_col){

  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(by_col)
  checkmate::assert_function(decay_function)

  checkmate::assert_names(names(data), must.include = opportunity_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = travel_cost_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_col, .var.name = "data")

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  access <- data[, .(access = sum( get(opportunity_col) * decay_function(t_ij=get(travel_cost_col)))),
                 by= c(by_col)]

  return(access)
}
