#' Gravity-based accessibility measures
#'
#' The function calculates gravity-based accessibility measures...
#'
#' @param data A `data.frame` with a travel time matrix in long format,
#'   containing the at least the columns of origin `from_id`, destination `to_id`,
#'   travel time `travel_time` from origin to destination, and number of
#'   opportunities in destination locations.
#' @param opportunity_colname A `string` indicating the name of the column with
#'   data on the opportunities to be considered.
#' @param by_colname A `string` with the name of the column of origin or
#'   destination that should be considered, indicating whether accessibility
#'   levels should by calculated by each origin (active accessibility) or
#'   destination (passive accessibility).
#' @param decay_function A string. Which decay function to use when calculating
#'            accessibility. One of step, exponential, fixed_exponential, linear
#'            or logistic. Please see the details to understand how each
#'            alternative works and how they relate to the `cutoffs` and
#'            `decay_value` parameters.
#' @param cutoff A numeric vector. This parameter has different effects for each
#'               decay function: it indicates the cutoff times in minutes when
#'               calculating cumulative opportunities accessibility with the
#'               `step` function...
#' @param decay_value A number. Extra parameter to be passed to the selected
#'               `decay_function`. Has no effects when `decay_function` is either
#'               `step` or `exponential`.
#'
#' @return A `data.table` object.
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df_linear <- gravity_access(data = ttm,
#'                            opportunity_colname = 'schools',
#'                            by_colname = 'from_id',
#'                            decay_function = 'linear',
#'                            cutoff = 30)
#'
#'head(df_linear)
#'
#'df_neg_exp <- gravity_access(data = ttm,
#'                             opportunity_colname = 'schools',
#'                             by_colname = 'from_id',
#'                             decay_function = 'negative_exponential',
#'                             decay_value = 0.5)
#'head(df_neg_exp)
#'
#' @family Cumulative access
#' @export
gravity_access <- function(data,
                           opportunity_colname,
                           by_colname,
                           decay_function,
                           cutoff=NULL,
                           decay_value=NULL){

  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_string(opportunity_colname)
  checkmate::assert_string(by_colname)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_colname,
                          .var.name = "data")


  # calculate access -----------------------------------------------------------


  # eval colnames
  opport_colname_ref <- as.name(opportunity_colname)
  by_colname_ref <- as.name(by_colname)
  data.table::setDT(data)

  access <- data[, .(access = sum(eval(opport_colname_ref) * impedance_fun(t_ij = travel_time, decay_function = decay_function, cutoff, decay_value))),
                 by=eval(by_colname)]

  return(access)
}
