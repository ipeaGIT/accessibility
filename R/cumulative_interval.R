#' Cumulative access based on maximum travel time interval
#'
#' Calculates the average or median number of opportunities that can be reached
#' considering multiple minute-by-minute maximum travel cost thresholds within
#' a given travel cost interval specified by the user. The time interval
#' cumulative accessibility measures was originally proposed by Tomasiello et
#' al. (*forthcoming*).
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @param interval A `numeric` vector of length 2. Indicates the start and end
#'   points of the interval of travel cost thresholds to be used. The first
#'   entry must be lower than the second.
#' @param summary_function A function. This function is used to summarize a
#'   distribution of accessibility estimates within a travel cost interval as a
#'   single value. Can be any function that takes an arbitrary number of
#'   numeric values as as input and returns a single number as output. Defaults
#'   to [stats::median()].
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
#' @template active
#'
#' @template return_accessibility
#'
#' @family cumulative access
#'
#' @details
#' ## References:
#' - Tomasiello, D. B.; Herszenhut, D.; Oliveira, J. L. A.; Braga, C. K. V.;
#' Pereira, R. H. M. (*forthcoming*). A time interval metric for cumulative
#' opportunity accessibility.
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' df <- cumulative_interval(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   interval = c(20, 30),
#'   opportunity_col = "schools",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' df <- cumulative_interval(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   interval = c(40, 80),
#'   opportunity_col = "jobs",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' @export
cumulative_interval <- function(travel_matrix,
                                land_use_data,
                                interval,
                                summary_function = stats::median,
                                opportunity_col,
                                travel_cost_col = "travel_time",
                                by_col = NULL,
                                active = TRUE) {
  by_col_char <- assert_and_assign_by_col(by_col)
  checkmate::assert_numeric(
    interval,
    lower = 0,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    sorted = TRUE,
    finite = TRUE
  )
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)
  assert_summary_function(summary_function)
  assert_travel_matrix(travel_matrix, travel_cost_col, by_col_char)
  assert_land_use_data(land_use_data, opportunity_col)

  # if not a dt, keep original class to assign later when returning result

  if (!inherits(travel_matrix, "data.table")) {
    original_class <- class(travel_matrix)
    data <- data.table::as.data.table(travel_matrix)
  } else {
    data <- data.table::copy(travel_matrix)
  }

  if (!inherits(land_use_data, "data.table")) {
    land_use_data <- data.table::as.data.table(land_use_data)
  }

  # small optimization: we can ditch anything that costs more than the upper
  # limit of the interval, since it won't affect the results anyway
  data <- data[get(travel_cost_col) <= interval[2]]

  merge_by_reference(data, land_use_data, opportunity_col, active)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, by_col_char)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost_col, group_id, groups)

  cutoffs <- seq.int(interval[1], interval[2])
  names(cutoffs) <- as.character(cutoffs)

  access <- lapply(
    cutoffs,
    function(x) {
      data[
        get(travel_cost_col) <= x,
        .(access = sum(get(opportunity_col))),
        by = eval(groups, envir = env)
      ]
    }
  )
  access <- data.table::rbindlist(access, idcol = "cutoffs")

  access <- access[
    ,
    .(access = as.numeric(summary_function(access))),
    by = eval(groups, envir = env)
  ]

  return(access)
}
