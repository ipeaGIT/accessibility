#' @title Minimum travel time to closest opportunities
#'
#' @description
#' The function calculates the minimum travel time to closest N number of
#' opportunities.
#'
#' @template input_data
#' @template opportunity_col
#' @param by_col A `string` with the name of the column of origin or
#'   destination that should be considered, indicating whether accessibility
#'   levels should by calculated by each origin (active accessibility) or
#'   destination (passive accessibility).
#' @param n_opportunities A `numeric` value with the mininum N number of
#'   opportunities that should be considered. Defaults to `1`
#'

#' @return A `data.table` object indicating for each origin the travel time to
#'         the closest opportunity and the id of the destination where it is
#'         located.
#' @family Minimum travel time
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_col = 'schools',
#'                      by_col = 'from_id',
#'                      n_opportunities = 1)
#'head(df)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_col = 'schools',
#'                      by_col = 'from_id',
#'                      n_opportunities = 2)
#'head(df)
#'
#' @export
time_to_closest <- function(data, opportunity_col, by_col, n_opportunities = 1){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_col)
  checkmate::test_string(by_col)
  checkmate::assert_number(n_opportunities, lower = 1, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_col,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)


 if (n_opportunities == 1) {

   access <- data[ get(opportunity_col) > 0,
                   .(travel_time = min(travel_time[which(get(opportunity_col) > 0 )])
                     , destination = to_id[which.min(travel_time)]
                     ), by = c(by_col)]

 } else {

  # keep only destinations with at least one opportunity
  temp <- data[ get(opportunity_col) > 0,]

  # sort by shortest to longets travel times
  temp <- temp[order(get(by_col), travel_time)]

  # cumsum of opportunities
  temp[, cum_opport := cumsum(get(opportunity_col)), by = c(by_col)]

  access <- temp[,
                 .(travel_time = travel_time[which(cum_opport == n_opportunities)]
                   , destination = paste(to_id[which(cum_opport <= n_opportunities)], collapse  = ";")
                 ), by = c(by_col)]
  }

  return(access)
}

