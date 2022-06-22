#' @title Gravity-based accessibility measures
#'
#' @description
#' The function calculates gravity-based accessibility measures...
#'
#' @param data A `data.frame` with a travel time matrix in long format,
#'   containing the at least the columns of origin `from_id`, destination `to_id`,
#'   travel time `travel_time` from origin to destination, and number of
#'   opportunities in destination locations.
#' @param opportunity_col A `string` indicating the name of the column with
#'   data on the opportunities to be considered.
#' @param by_col A `string` with the name of the column of origin or
#'   destination that should be considered, indicating whether accessibility
#'   levels should by calculated by each origin (active accessibility) or
#'   destination (passive accessibility).
#' @param decay_function A `fuction` that converts travel cost into and impedance
#'   factor used to weigth opportunities. For convinence, the package currently
#'   includes the following functions: `decay_bineary`, `decay_linear`,
#'   `decay_exponential` and `decay_power.` See the documentation of
#'   each function for more details.
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
#'                            by_col = 'from_id',
#'                            decay_function = decay_linear(cutoff = 50)
#'                            )
#'
#'head(df_linear)
#'
#'df_exp <- gravity_access(data = ttm,
#'                         opportunity_col = 'schools',
#'                         by_col = 'from_id',
#'                         decay_function = decay_exponential(decay_value = 0.5)
#'                         )
#'head(df_exp)
#'
#' @export
gravity_access <- function(data,
                           opportunity_col,
                           by_col,
                           decay_function){

  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(by_col)
  checkmate::assert_function(decay_function)

  checkmate::assert_names(names(data), must.include = opportunity_col,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  access <- data[, .(access = sum( get(opportunity_col) * decay_function(t_ij=travel_time))),
                 by= c(by_col)]

  return(access)
}
