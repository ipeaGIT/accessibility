#' Cumulative access based on time threshold
#'
#' The function calculates the number of opportunities accessible under a given
#' travel time threshold specified by the user.
#'
#' @param data A `data.frame` with a travel time matrix in long format.
#' @param opportunity_colname A `string` indicating the column name where the
#' data on number of opportunities is stored.
#' @param cutoff A `numeric` value indicating the maximum travel time considered.
#' @param by_col A `string` pointing to the name of the column of origin or
#' destination.
#'
#' @return A `data.table` object.
#'
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_poa.csv", package = "accessibility")
#' ttm <- read.csv(data_path)
#'
#'# Active accessibility: number of schools accessible from each origin
#'df <- cumulative_time_threshold(data = ttm,
#'                                opportunity_colname = 'schools',
#'                                cutoff = 30,
#'                                by_col = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_threshold(data = ttm,
#'                                opportunity_colname = 'population',
#'                                cutoff = 30,
#'                                by_col = 'to_id')
#'head(df)
#' @family Cumulative access
#' @export
cumulative_time_threshold <- function(data, opportunity_colname, cutoff = 20, by_col='from_id'){


  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_col)
  checkmate::assert_number(cutoff, lower = 0)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")


  # calculate access -----------------------------------------------------------

  data.table::setDT(data)

  ### TO DO
  # CONVERT the "travel_time" column into a function parameter ?
  colname <- as.name(opportunity_colname)
  access <- data[travel_time <= cutoff, .(access = sum(eval(colname))), by=by_col]

  return(access)
}
