#' @title Cumulative access based on maximum travel time interval
#'
#' @description
#' The function calculates the average or median number of opportunities that
#' can be reached considering multiple minute-by-minute maximum travel time
#' thresholds within a given travel time interval specified by the user.
#'
#' @template input_data
#' @template opportunity_col
#' @param by_col A `string` with the name of the column of origin or
#'   destination that should be considered, indicating whether accessibility
#'   levels should by calculated by each origin (active accessibility) or
#'   destination (passive accessibility).
#' @param interval An `numeric vector` of length 2, indicating the start and end
#'   points of the interval of travel time thresholds to be used.
#' @param stat A `string` indicating the summary statistic used to aggregate the
#' accessibility estimates within the time interval. It accepts either `median`
#' (Default) or `mean`.
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
#'                               opportunity_col = 'schools',
#'                               interval = c(20, 30),
#'                               by_col = 'from_id',
#'                               stat ='mean')
#'head(df)
#'
#' @export
cumulative_time_interval <- function(data, opportunity_col, interval, by_col, stat='mean'){

  # check inputs ------------------------------------------------------------
  checkmate::test_data_frame(data)
  checkmate::test_string(opportunity_col)
  checkmate::test_string(by_col)

  checkmate::assert_vector(interval, any.missing = FALSE, len = 2 ,unique = TRUE)
  checkmate::assert_number(interval[1], lower = 0, finite = TRUE)
  checkmate::assert_number(interval[2], lower = 0, finite = TRUE)

  checkmate::assert_names(names(data), must.include = opportunity_col,
                          .var.name = "data")

  checkmate::assert_names(names(data), must.include = by_col,
                          .var.name = "data")

  checkmate::assert_choice(x=stat, choices=c('mean', 'median'))



  # calculate access -----------------------------------------------------------

  # minute-by-minute interval
  interval
  vct <- interval[1]:interval[2]

  # calculate cumulative access for every minute in the interval
  access_list <- lapply(X=vct,
                        FUN= function(i){
                          temp <-  cumulative_time_cutoff(data = data,
                                                          cutoff = i,
                                                          opportunity_col = opportunity_col,
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
