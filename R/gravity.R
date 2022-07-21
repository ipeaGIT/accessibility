#' Gravity-based accessibility measures
#'
#' Calculates gravity-based accessibility using a decay function specified by
#' the user.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @template decay_function
#' @template group_by
#' @template active
#' @template fill_missing_ids_combinations
#'
#' @template return_accessibility
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' df_linear <- gravity(
#'   travel_matrix,
#'   land_use_data,
#'   decay_function = decay_linear(cutoff = 50),
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df_linear)
#'
#' df_exp <- gravity(
#'   travel_matrix,
#'   land_use_data,
#'   decay_function = decay_exponential(decay_value = 0.5),
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df_exp)
#'
#' @export
gravity <- function(travel_matrix,
                    land_use_data,
                    opportunity,
                    travel_cost,
                    decay_function,
                    group_by = character(0),
                    active = TRUE,
                    fill_missing_ids = TRUE) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)
  checkmate::assert_logical(fill_missing_ids, len = 1, any.missing = FALSE)
  assert_decay_function(decay_function)
  assert_group_by(group_by)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(land_use_data, opportunity)

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

  merge_by_reference(data, land_use_data, opportunity, active)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, group_by)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost, group_id, groups)

  .opp_colname <- opportunity
  .cost_colname <- travel_cost
  access <- data[
    ,
    .(
      access = sum(get(.opp_colname) * decay_function(get(.cost_colname)))
    ),
    by = eval(groups, envir = env)
  ]

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    names(unique_values) <- groups
    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- do_fill_missing_ids(access, possible_combinations, groups)
    }
  }

  data.table::setnames(access, c(group_id, "access"), c("id", opportunity))

  if (exists("original_class")) class(access) <- original_class

  return(access)
}
