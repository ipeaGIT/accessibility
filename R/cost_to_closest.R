#' Minimum travel cost to closest N number of opportunities
#'
#' Calculates the minimum travel cost to the closest N number of opportunities.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @param n A `numeric`. A number indicating the minimum number of opportunities
#'   that should be considered. Defaults to 1.
#' @template opportunity_col
#' @template travel_cost_col
#' @template by_col
#' @template active
#' @param fill_missing_ids A `logical`. Calculating minimum travle cost to
#'   closest N number of opportunities may result in missing ids in the output
#'   if they cannot reach the specified amount of opportunities across all
#'   destinations they can reach. When `TRUE` (the default), the function
#'   identifies which origins would be left out from the output and fill their
#'   respective minimum travel costs with `Inf` and the `destination` column
#'   with `NA`.
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
#'   opportunity_col = "schools",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' df <- cost_to_closest(
#'   travel_matrix,
#'   land_use_data,
#'   n = 2,
#'   opportunity_col = "schools",
#'   travel_cost_col = "travel_time"
#' )
#' head(df)
#'
#' @export
cost_to_closest <- function(travel_matrix,
                            land_use_data,
                            n = 1,
                            opportunity_col,
                            travel_cost_col = "travel_time",
                            by_col = NULL,
                            active = TRUE,
                            fill_missing_ids = TRUE) {
  by_col_char <- assert_and_assign_by_col(by_col)
  checkmate::assert_number(n, lower = 1, finite = TRUE)
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

  merge_by_reference(data, land_use_data, opportunity_col, active)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, by_col_char)
  dest_or_orig_id <- setdiff(c("from_id", "to_id"), group_id)
  env <- environment()

  warn_extra_cols(travel_matrix, travel_cost_col, group_id, groups)

  if (n == 1) {
    access <- data[
      data[
        get(opportunity_col) > 0,
        .I[get(travel_cost_col) == suppressWarnings(min(get(travel_cost_col)))],
        by = eval(groups, envir = env)
      ]$V1
    ]
    access <- access[
      ,
      .(
        min_cost = get(travel_cost_col)[1],
        dest_or_orig = paste(eval(as.name(dest_or_orig_id)), collapse = ";")
      ),
      by = eval(groups, envir = env)
    ]
  } else {
    opport_cumsum <- data[get(opportunity_col) > 0, ]
    data.table::setorderv(opport_cumsum, c(groups, travel_cost_col))
    opport_cumsum[
      ,
      cum_opport := cumsum(get(opportunity_col)),
      by = eval(groups, envir = env)
    ]

    access <- opport_cumsum[
      ,
      .(
        min_cost = suppressWarnings(min(get(travel_cost_col)[cum_opport >= n])),
        dest_or_orig = paste(
          eval(as.name(dest_or_orig_id))[cum_opport <= n],
          collapse  = ";"
        )
      ),
      by = eval(groups, envir = env)
    ]

    if (fill_missing_ids) {
      access[is.infinite(min_cost), dest_or_orig := NA]
    } else {
      access <- access[is.finite(min_cost)]
    }
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

  dest_or_orig_name <- ifelse(
    dest_or_orig_id == "to_id",
    "destination",
    "origin"
  )

  data.table::setnames(
    access,
    c(group_id, "min_cost", "dest_or_orig"),
    c("id", travel_cost_col, dest_or_orig_name)
  )

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}

