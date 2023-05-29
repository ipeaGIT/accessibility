#' Cumulative access based on a travel cost cutoff
#'
#' Calculates the number of opportunities accessible under a given specified
#' travel cost cutoff.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @param cutoff A `numeric` vector. The travel cost cutoffs to consider when
#'   calculating accessibility levels. If more than one value is provided, the
#'   output includes an extra column specifying the cutoff that the
#'   accessibility levels refer to.
#' @template group_by
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
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' df <- cumulative_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = c(30, 60),
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' # passive accessibility: number of people that can reach each destination
#' df <- cumulative_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = 30,
#'   opportunity = "population",
#'   travel_cost = "travel_time",
#'   active = FALSE
#' )
#' head(df)
#'
#' @export
cumulative_cutoff <- function(travel_matrix,
                              land_use_data,
                              opportunity,
                              travel_cost,
                              cutoff,
                              group_by = character(0),
                              active = TRUE,
                              fill_missing_ids = TRUE) {
  checkmate::assert_numeric(
    cutoff,
    lower = 1,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_logical(active, len = 1, any.missing = FALSE)
  checkmate::assert_logical(fill_missing_ids, len = 1, any.missing = FALSE)
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

  merge_by_reference(
    data,
    land_use_data,
    opportunity,
    left_df_idcol = ifelse(active, "to_id", "from_id")
  )

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, group_by)
  warn_extra_cols(travel_matrix, travel_cost, group_id, groups)

  access <- lapply(
    cutoff,
    function(.x) calc_cum_cutoff(data, groups, opportunity, travel_cost, .x)
  )
  names(access) <- cutoff
  access <- data.table::rbindlist(access, idcol = "cutoff")
  access[, cutoff := as.numeric(cutoff)]

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    unique_values <- append(unique_values, list(cutoff))
    names(unique_values) <- c(groups, "cutoff")

    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- do_fill_missing_ids(
        access,
        possible_combinations,
        groups = c(groups, "cutoff")
      )
    }
  }

  data.table::setnames(access, c(group_id, "access"), c("id", opportunity))
  data.table::setcolorder(
    access,
    c("id", setdiff(groups, group_id), "cutoff", opportunity)
  )
  data.table::setorderv(access, c("id", setdiff(groups, group_id), "cutoff"))
  if (length(cutoff) == 1) access[, cutoff := NULL]

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}


calc_cum_cutoff <- function(data, groups, opportunity, travel_cost, .cutoff) {
  env <- environment()
  .opportunity_colname <- opportunity
  .cost_colname <- travel_cost

  data <- data[get(.cost_colname) <= .cutoff]

  cum_cutoff_access <- data[
    ,
    .(access = sum(get(.opportunity_colname))),
    by = eval(groups, envir = env)
  ]

  return(cum_cutoff_access)
}
