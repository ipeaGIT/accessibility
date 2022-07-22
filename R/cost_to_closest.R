#' Minimum travel cost to closest N number of opportunities
#'
#' Calculates the minimum travel cost to the closest N number of opportunities.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @param n A `numeric`. A number indicating the minimum number of opportunities
#'   that should be considered. Defaults to 1.
#' @template group_by
#' @template active
#' @param fill_missing_ids A `logical`. Calculating minimum trave cost to
#'   closest N number of opportunities may result in missing ids in the output
#'   if they cannot reach the specified amount of opportunities across all
#'   destinations they can reach. For example, estimating the minimum travel
#'   time that an origin that can only reach 4 opportunities takes to reach 5
#'   opportunities resulting in such origin not being included in the output.
#'   When `TRUE` (the default), the function identifies which ids would be left
#'   out from the output and fill their respective minimum travel costs with
#'   `Inf`, which incurs in a performance penalty.
#'
#' @template return_accessibility
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' df <- cost_to_closest(
#'   travel_matrix,
#'   land_use_data,
#'   n = 1,
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' df <- cost_to_closest(
#'   travel_matrix,
#'   land_use_data,
#'   n = 2,
#'   opportunity = "schools",
#'   travel_cost = "travel_time"
#' )
#' head(df)
#'
#' @export
cost_to_closest <- function(travel_matrix,
                            land_use_data,
                            opportunity,
                            travel_cost,
                            n = 1,
                            group_by = character(0),
                            active = TRUE,
                            fill_missing_ids = TRUE) {
  checkmate::assert_number(n, lower = 1, finite = TRUE)
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

  merge_by_reference(data, land_use_data, opportunity, active)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, group_by)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost, group_id, groups)

  .opportunity_colname <- opportunity
  .cost_colname <- travel_cost
  if (n == 1) {
    access <- data[
      get(.opportunity_colname) > 0,
      .(min_cost = suppressWarnings(min(get(.cost_colname)))),
      by = eval(groups, envir = env)
    ]
  } else {
    opport_cumsum <- data[get(.opportunity_colname) > 0, ]
    data.table::setorderv(opport_cumsum, c(groups, travel_cost))
    opport_cumsum[
      ,
      cum_opport := cumsum(get(.opportunity_colname)),
      by = eval(groups, envir = env)
    ]

    access <- opport_cumsum[
      cum_opport >= n,
      .(min_cost = suppressWarnings(min(get(.cost_colname)))),
      by = eval(groups, envir = env)
    ]
  }

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    names(unique_values) <- groups
    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- do_fill_missing_ids(
        access,
        possible_combinations,
        groups,
        access_col = "min_cost",
        fill_value = Inf
      )
    }
  }

  data.table::setnames(
    access,
    c(group_id, "min_cost"),
    c("id", travel_cost)
  )

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}
