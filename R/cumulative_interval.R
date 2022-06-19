#' @title Cumulative access based on time interval
#'
#' @description
#' The function calculates the average or median number of opportunities that
#' can be reached considering multiple minute-by-minute time thresholds within a
#' given travel time interval specified by the user.
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
#' @param start An `integer` indicating the `start` point of the travel time
#' interval.
#' @param end An `integer` indicating the `end` point of the travel time interval.
#' @param stat A `string` indicating the summary statistic used to aggregate the
#' accessibility estimates within the time interval. It accepts either `median`
#' (Default) or `mean`.
#'
#' @return A `data.table` object.
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'df <- cumulative_time_interval(data = ttm,
#'                               opportunity_colname = 'schools',
#'                               start = 20,
#'                               end = 30,
#'                               by_colname = 'from_id',
#'                               stat ='median')
#'head(df)
#'
#' @family Cumulative access
#' @export
cumulative_time_interval <- function(data, opportunity_colname, start, end, by_colname, stat='mean'){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_colname)
  checkmate::test_string(by_colname)
  checkmate::assert_number(start, lower = 0, finite = TRUE)
  checkmate::assert_number(end, lower = 0, finite = TRUE)


  checkmate::assert_names(names(data), must.include = opportunity_colname,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_colname,
                          .var.name = "data")

  checkmate::assert_choice(x=stat, choices=c('mean', 'median'))



  # calculate access -----------------------------------------------------------

  # minute-by-minute interval
  # vct <- seq(start, end, 1)
  vct <- start:end

  # calculate cumulative access for every minute in the interval
  access_list <- lapply(X=vct,
                        FUN= function(i){
                          temp <-  cumulative_time_cutoff(data = data,
                                                          cutoff = i,
                                                          opportunity_colname = opportunity_colname,
                                                          by_colname = by_colname)
                          return(temp)
                        }
  )
  access <- data.table::rbindlist(access_list)



  # summary measure to be used
  if (stat=='mean') {
    access <- access[, .(access = mean(access, na.rm=T)), by = c(by_colname) ] }

  if (stat=='median') {
    access <- access[, .(access = median(access, na.rm=T)), by = c(by_colname) ] }

  return(access)
}
