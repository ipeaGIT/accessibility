#' @title Cumulative access based on maximum travel time cutoff
#'
#' @description
#' The function calculates the number of opportunities accessible under a given
#' travel time cutoff specified by the user.
#'
#' @template input_data
#' @template opportunity_col
#' @template by_col
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
