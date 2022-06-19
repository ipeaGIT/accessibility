#' @title Cumulative access based on maximum travel time cutoff
#'
#' @description
#' The function calculates the number of opportunities accessible under a given
#' travel time cutoff specified by the user.
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
#' @param cutoff A `numeric` value indicating the maximum travel time considered.
#'
#' @return A `data.table` object.
#' @family Cumulative access
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'# Active accessibility: number of schools accessible from each origin
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_col = 'schools',
#'                             cutoff = 30,
#'                             by_col = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_col = 'population',
#'                             cutoff = 30,
#'                             by_col = 'to_id')
#'head(df)
#' @export
cumulative_time_cutoff <- function(data, opportunity_col, cutoff, by_col){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_col)
  checkmate::test_string(by_col)
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_col,
                          .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  access <- data[travel_time <= cutoff, .(access = sum(get(opportunity_col)) ), by=c(by_col)]

  return(access)
}
