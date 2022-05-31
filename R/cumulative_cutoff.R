#' Cumulative access based on time threshold
#'
#' The function calculates the number of opportunities accessible under a given
#' travel time threshold specified by the user.
#'
#' @param data A `data.frame` with a travel time matrix in long format
#' @param cutoff An `numeric`
#' @param by_col A string pointing to the name of the column of origin or destination
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
#'                                cutoff = 30,
#'                                by_col = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_threshold(data = ttm,
#'                                cutoff = 30,
#'                                by_col = 'to_id')
#' @family Cumulative access
#' @export
cumulative_time_threshold <- function(data, cutoff = 20, by_col='from_id'){

  checkmate::test_numeric(cutoff)
  checkmate::test_data_frame(data)

  data.table::setDT(data)

  ### TO DO
  # CONVERT the "schools" columns into a parameter
  # CONVERT the "travel_time" columns into a parameter
  access <- data[travel_time <= cutoff, .(access = sum(schools)), by=by_col]

  return(access)
}
