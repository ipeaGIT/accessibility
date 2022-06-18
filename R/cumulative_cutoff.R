#' Cumulative access based on maximum travel time cutoff
#'
#' The function calculates the number of opportunities accessible under a given
#' travel time cutoff specified by the user.
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
#' @param cutoff A `numeric` value indicating the maximum travel time considered.
#'
#' @return A `data.table` object.
#'
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'# Active accessibility: number of schools accessible from each origin
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_colname = 'schools',
#'                             cutoff = 30,
#'                             by_colname = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_colname = 'population',
#'                             cutoff = 30,
#'                             by_colname = 'to_id')
#'head(df)
#' @family Cumulative access
#' @export
cumulative_time_cutoff <- function(data, opportunity_colname, cutoff, by_colname){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_colname)
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_colname,
                          .var.name = "data")


  # calculate access -----------------------------------------------------------

  # eval colnames
  opport_colname_ref <- as.name(opportunity_colname)
  by_colname_ref <- as.name(by_colname)
  data.table::setDT(data)

  access <- data[travel_time <= cutoff, .(access = sum(eval(opport_colname_ref))), by=eval(by_colname)]

  return(access)
}
