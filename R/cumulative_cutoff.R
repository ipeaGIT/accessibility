#' Cumulative access based on a travel cost cutoff
#'
#' Calculates the number of opportunities accessible under a given specified
#' travel cost cutoff.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff.
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
#' @template active
#' @param fill_missing_ids A `logical`. Calculating cumulative accessibility may
#'   result in missing ids if the they cannot reach any of the destinations
#'   within the specified travel cost cutoff. For example, using a travel time
#'   cutoff of 20 minutes, when estimating the accessibility of origin `A` that
#'   can only reach destinations with more than 40 minutes results in id `A`
#'   not being included in the output. When `TRUE` (the default), the function
#'   identifies which origins would be left out and fills their respective
#'   accessibility values with 0, which incurs in a performance penalty.
#'
#' @template return_accessibility
#'
#' @family cumulative access
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' # active accessibility: number of schools accessible from each origin
#' df <- cumulative_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = 30,
#'   opportunity_col = "schools",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' # passive accessibility: number of people that can reach each destination
#' df <- cumulative_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = 30,
#'   opportunity_col = "population",
#'   travel_cost_col = "travel_time",
#'   active = FALSE
#' )
#' head(df)
#'
#' @export
cumulative_cutoff <- function(travel_matrix,
                              land_use_data,
                              cutoff,
                              opportunity_col,
                              travel_cost_col = "travel_time",
                              by_col = NULL,
                              active = TRUE,
                              fill_missing_ids = TRUE) {
  by_col_char <- assert_and_assign_by_col(by_col)
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)
  checkmate::assert_logical(fill_missing_ids, len = 1, any.missing = FALSE)
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

  data <- data[get(travel_cost_col) <= cutoff]
  merge_by_reference(data, land_use_data, opportunity_col, active)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, by_col_char)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost_col, group_id, groups)

  access <- data[
    ,
    .(access = sum(get(opportunity_col))),
    by = eval(groups, envir = env)
  ]

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    names(unique_values) <- groups
    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- fill_missing_ids(access, possible_combinations, groups)
    }
  }

  data.table::setnames(access, c(group_id, "access"), c("id", opportunity_col))

  if (exists("original_class")) class(access) <- original_class

  return(access)
}
