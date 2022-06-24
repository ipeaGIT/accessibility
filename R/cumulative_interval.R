#' @title Cumulative access based on maximum travel time interval
#'
#' @description
#' The function calculates the average or median number of opportunities that
#' can be reached considering multiple minute-by-minute maximum travel time
#' thresholds within a given travel time interval specified by the user.
#'
#' @template input_data
#' @param interval An `numeric vector` of length 2, indicating the start and end
#'   points of the interval of travel time thresholds to be used.
#' @param stat A `string` indicating the summary statistic used to aggregate the
#' accessibility estimates within the time interval. It accepts either `median`
#' (Default) or `mean`.
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
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
#'df <- cumulative_time_interval(data = ttm,
#'                               interval = c(20, 30),
#'                               stat ='mean',
#'                               opportunity_col = 'schools',
#'                               travel_cost_col='travel_time',
#'                               by_col = 'from_id')
#'head(df)
#'
#'df <- cumulative_time_interval(data = ttm,
#'                               interval = c(40, 80),
#'                               stat ='mean',
#'                               opportunity_col = 'jobs',
#'                               travel_cost_col='travel_time',
#'                               by_col = 'from_id')
#'head(df)
#'
#' @export
cumulative_time_interval <- function(data, interval, stat='mean', opportunity_col, travel_cost_col='travel_time', by_col){

  # check inputs ------------------------------------------------------------

  checkmate::assert_vector(interval, any.missing = FALSE, len = 2 ,unique = TRUE)
  checkmate::assert_number(interval[1], lower = 0, finite = TRUE)
  checkmate::assert_number(interval[2], lower = 0, finite = TRUE)
  checkmate::assert_choice(x=stat, choices=c('mean', 'median'))

    # all other string tests are checked inside cumulative_time_cutoff()


  # calculate access -----------------------------------------------------------

  # minute-by-minute interval
  interval # do not remove this line
  vct <- interval[1]:interval[2]

  # calculate cumulative access for every minute in the interval
  access_list <- lapply(X=vct,
                        FUN= function(i){
                          temp <-  cumulative_time_cutoff(data = data,
                                                          cutoff = i,
                                                          opportunity_col = opportunity_col,
                                                          travel_cost_col = travel_cost_col,
                                                          by_col = by_col)
                          return(temp)
                        }
  )
  access <- data.table::rbindlist(access_list)



  # summary measure to be used
  if (stat=='mean') {
    access <- access[, .(access = mean(access, na.rm=T)), by = c(by_col) ] }

  if (stat=='median') {
    access <- access[, .(access = median(access, na.rm=T)), by = c(by_col) ] }

  return(access)
}
