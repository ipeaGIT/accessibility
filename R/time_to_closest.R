#' Minimum travel time to closest opportunity
#'
#' The function calculates the minimum travel time to closest opportunity.
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
#'
#' @return A `data.table` object indicating for each origin the travel time to
#' the closest opportunity and the id of the destination where it is located.
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_poa.csv", package = "accessibility")
#' ttm <- read.csv(data_path)
#'
#'df <- time_to_closest(data = ttm,
#'                               opportunity_colname = 'schools',
#'                               by_colname = 'from_id')
#'head(df)
#'
#' @family Minimum travel time
#' @export
time_to_closest <- function(data, opportunity_colname, by_colname){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_colname)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_colname,
                          .var.name = "data")

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  colname <- as.name(opportunity_colname)
  access <- data[ eval(colname) > 0,
                  .(travel_time = min(travel_time[which(eval(colname) > 0 )])
                   , destination = to_id[which.min(travel_time)]

                    ), by=by_colname]

  return(access)
}

