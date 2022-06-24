#' @title Cumulative access based on maximum travel time cutoff
#'
#' @description
#' The function calculates the number of opportunities accessible under a given
#' travel time cutoff specified by the user.
#'
#' @template input_data
#' @param cutoff A `numeric` value indicating the maximum travel time considered.
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
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
#'                             cutoff = 30,
#'                             opportunity_col = 'schools',
#'                             travel_cost_col = 'travel_time',
#'                             by_col = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_cutoff(data = ttm,
#'                             cutoff = 30,
#'                             opportunity_col = 'population',
#'                             travel_cost_col = 'travel_time',
#'                             by_col = 'to_id')
#'head(df)
#' @export
cumulative_time_cutoff <- function(data, cutoff, opportunity_col, travel_cost_col='travel_time', by_col){

  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_string(by_col)
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = travel_cost_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_col, .var.name = "data")


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  access <- data[get(travel_cost_col) <= cutoff, .(access = sum(get(opportunity_col)) ), by=c(by_col)]

  return(access)
}
