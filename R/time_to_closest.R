#' @title Minimum travel cost to closest N number of opportunities
#'
#' @description
#' The function calculates the minimum travel cost to closest N number of
#' opportunities.
#'
#' @template input_data
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
#' @param n_opportunities A `numeric` value with the minimum N number of
#'   opportunities that should be considered. Defaults to `1`
#'
#' @return A `data.table` object indicating for each origin the travel time to
#'         the closest opportunity and the id of the destination where it is
#'         located.
#' @family Minimum travel cost
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_col = 'schools',
#'                      n_opportunities = 1,
#'                      travel_cost_col = 'travel_time',
#'                      by_col = 'from_id')
#'head(df)
#'
#'df <- time_to_closest(data = ttm,
#'                      opportunity_col = 'schools',
#'                      n_opportunities = 2,
#'                      travel_cost_col = 'travel_time',
#'                      by_col = 'from_id')
#'head(df)
#'
#' @export
time_to_closest <- function(data, opportunity_col, n_opportunities = 1, travel_cost_col = 'travel_time', by_col){

  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_string(by_col)
  checkmate::assert_number(n_opportunities, lower = 1, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = travel_cost_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = by_col, .var.name = "data")


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)


 if (n_opportunities == 1) {

   access <- data[ get(opportunity_col) > 0,
                   .(travel_cost = min(get(travel_cost_col)[which(get(opportunity_col) > 0 )])
                     , destination = to_id[which.min( get(travel_cost_col) )]
                     ), by = c(by_col)]

 } else {

  # keep only destinations with at least one opportunity
  temp <- data[ get(opportunity_col) > 0,]

  # sort by shortest to longets travel times
  temp <- temp[order(get(by_col), get(travel_cost_col))]

  # cumsum of opportunities
  temp[, cum_opport := cumsum(get(opportunity_col)), by = c(by_col)]

  access <- temp[,
                 .(travel_cost = get(travel_cost_col)[which(cum_opport == n_opportunities)]
                   , destination = paste(to_id[which(cum_opport <= n_opportunities)], collapse  = ";")
                 ), by = c(by_col)]
  }

  return(access)
}

