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
#'
#' @return A `data.table` containing the accessibility estimate for each origin
#'   in the travel matrix.
#'
#' @family Cumulative access
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' # active accessibility: number of schools accessible from each origin
#' df <- cumulative_time_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = 30,
#'   opportunity_col = "schools",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' # passive accessibility: number of people that can reach each destination
#' df <- cumulative_time_cutoff(
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
cumulative_time_cutoff <- function(travel_matrix,
                                   land_use_data,
                                   cutoff,
                                   opportunity_col,
                                   travel_cost_col = "travel_time",
                                   by_col = NULL,
                                   active = TRUE) {
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_string(by_col, null.ok = TRUE)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)

  by_col_char <- if (is.null(by_col)) {
    character(0)
  } else {
    by_col
  }

  checkmate::assert_names(
    by_col_char,
    disjunct.from = c("from_id", "to_id"),
    .var.name = "by_col"
  )

  travel_matrix_req_names <- c("from_id", "to_id", travel_cost_col, by_col_char)
  checkmate::assert_data_frame(travel_matrix)
  checkmate::assert_names(
    names(travel_matrix),
    must.include = travel_matrix_req_names,
    .var.name = "travel_matrix"
  )

  land_use_data_req_names <- c("id", opportunity_col)
  checkmate::assert_data_frame(land_use_data)
  checkmate::assert_names(
    names(land_use_data),
    must.include = land_use_data_req_names,
    .var.name = "land_use_data"
  )

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

  # when calculating active accessibility, we want the number of opportunities
  # in the destination. if passive, the number of opportunities in the origin

  join_id <- ifelse(active, "to_id", "from_id")
  join_vector <- "id"
  names(join_vector) <- join_id

  data <- data[get(travel_cost_col) <= cutoff]

  data[
    land_use_data,
    on = join_vector,
    eval(opportunity_col) := get(paste0("i.", opportunity_col))
  ]

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, by_col_char)
  env <- environment()

  access <- data[
    ,
    .(access = sum(get(opportunity_col))),
    by = eval(groups, envir = env)
  ]
  data.table::setnames(access, c(group_id, "access"), c("id", opportunity_col))

  if (exists("original_class")) class(access) <- original_class

  return(access)
}
