#' Cumulative access based on time interval
#'
#' The function calculates the average or median number of opportunities
#' accessible within a travel time interval specified by the user.
#'
#' @param data A `data.frame` with a travel time matrix in long format.
#' @param opportunity_colname A `string` indicating the column name where the
#' data on number of opportunities is stored.
#' @param start An `integer` indicating the `start` point of the travel time
#' interval.
#' @param end An `integer` indicating the `end` point of the travel time interval.
#' @param stat A `string` indicating the summary measure to be used. It
#' accepts either `median` (Default) or `mean`.
#' @param by_col A string pointing to the name of the column of origin or
#' destination.
#'
#' @return A `data.table` object.
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_poa.csv", package = "accessibility")
#' ttm <- read.csv(data_path)
#'
#'df <- cumulative_time_interval(data = ttm,
#'                               opportunity_colname = 'schools',
#'                               start = 20,
#'                               end = 30,
#'                               by_col = 'from_id',
#'                               stat ='median')
#'head(df)
#'
#' @family Cumulative access
#' @export
cumulative_time_interval <- function(data = df, opportunity_colname, start=20, end=30, by_col='from_id', stat='mean'){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_col)
  checkmate::test_numeric(start)
  checkmate::test_numeric(end)

  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")

  checkmate::assert_choice(x=stat, choices=c('mean', 'median'))



  # calculate access -----------------------------------------------------------

  # minute-by-minute interval
  vct <- seq(start, end, 1)

  # calculate cumulative access for every minute in the interval
  access_list <- lapply(X=vct,
                        FUN= function(i){
                          temp <-  cumulative_time_threshold(data = data,
                                                             cutoff = i,
                                                             opportunity_colname=opportunity_colname,
                                                             by_col=by_col)
                          return(temp)
                          }
                        )
  access <- data.table::rbindlist(access_list)

  # summary measure to be used
  if (stat=='mean') {
    access <- access[, .(access=mean(access, na.rm=T)), by=by_col ] }

  if (stat=='median') {
    access <- access[, .(access=median(access, na.rm=T)), by=by_col ] }

  return(access)
  }
