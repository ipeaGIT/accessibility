#' Minimum travel time to closest opportunities
#'
#' The function calculates the minimum travel time to closest N number of
#' opportunities.
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
#' @param n_opportunities A `numeric` value with the mininum N number of
#'   opportunities that should be considered. Defaults to `1`
#'
#'
#' @return A `data.table` object indicating for each origin the travel time to
#' the closest opportunity and the id of the destination where it is located.
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_colname = 'schools',
#'                      by_colname = 'from_id',
#'                      n_opportunities = 1)
#'head(df)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_colname = 'schools',
#'                      by_colname = 'from_id',
#'                      n_opportunities = 2)
#'head(df)
#'
#' @family Minimum travel time
#' @export
time_to_closest <- function(data, opportunity_colname, by_colname, n_opportunities = 1){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_colname)
  checkmate::assert_number(n_opportunities, lower = 1, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_colname,
                          .var.name = "data")

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)


 if (n_opportunities == 1) {

   access <- data[ get(opportunity_colname) > 0,
                   .(travel_time = min(travel_time[which(get(opportunity_colname) > 0 )])
                     , destination = to_id[which.min(travel_time)]
                     ), by = c(by_colname)]

 } else {

  # keep only destinations with at least one opportunity
  temp <- data[ get(opportunity_colname) > 0,]

  # sort by shortest to longets travel times
  temp <- temp[order(get(by_colname), travel_time)]

  # cumsum of opportunities
  temp[, cum_opport := cumsum(get(opportunity_colname)), by = c(by_colname)]

  access <- temp[,
                 .(travel_time = travel_time[which(cum_opport == n_opportunities)]
                   , destination = paste(to_id[which(cum_opport <= n_opportunities)], collapse  = ";")
                 ), by = c(by_colname)]
  }

  return(access)
}

