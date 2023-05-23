#' Balancing cost
#'
#' Balancing cost measure. Originally proposed by
#' \insertCite{barboza2021balancing;textual}{accessibility}.
#' @template description_generic_cost
#'
#' @template travel_matrix
#' @template land_use_data
#' @template opportunity
#' @template travel_cost
#' @template demand
#' @param max_lookup_cost A numeric.
#' @template group_by
#' @template fill_missing_ids_combinations
#'
#' @template return_accessibility
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' @export
balancing_cost <- function(travel_matrix,
                           land_use_data,
                           opportunity,
                           travel_cost,
                           demand,
                           max_lookup_cost,
                           group_by = character(0),
                           fill_missing_ids = TRUE) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(travel_cost)
  checkmate::assert_string(demand)
  checkmate::assert_number(max_lookup_cost, lower = 0, finite = TRUE) # act. >0
  checkmate::assert_logical(fill_missing_ids, len = 1, any.missing = FALSE)
  assert_group_by(group_by)
  assert_travel_matrix(travel_matrix, travel_cost, group_by)
  assert_land_use_data(land_use_data, opportunity, demand)

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

  merge_by_reference(data, land_use_data, opportunity, active = TRUE)

  group_id <- ifelse(active, "from_id", "to_id")
  groups <- c(group_id, group_by)
  warn_extra_cols(travel_matrix, travel_cost, group_id, groups)

  lookup_vec <- land_use_data[[demand]]
  names(lookup_vec) <- land_use_data$id

  lookup_costs <- seq.int(0, max_lookup_cost, by = 1)

  access <- data.table::data.table(from_id = character(), cost = numeric())
  for (i in lookup_costs) {
    cum_opps <- calc_cum_cutoff(data, groups, opportunity, travel_cost, i)

    cum_opps[, did_balance := access > lookup_vec[from_id]]
    balanced <- cum_opps[did_balance == TRUE]$from_id

    if (!identical(balanced, character(0))) {
      access <- rbind(
        access,
        data.table::data.table(from_id = balanced, cost = i)
      )
    }

    data <- data[!(from_id %in% balanced)]

    if (nrow(data) == 0) break
  }

  if (fill_missing_ids) {
    unique_values <- lapply(groups, function(x) unique(travel_matrix[[x]]))
    names(unique_values) <- groups

    possible_combinations <- do.call(data.table::CJ, unique_values)

    if (nrow(access) < nrow(possible_combinations)) {
      access <- do_fill_missing_ids(
        access,
        possible_combinations,
        groups = groups,
        access_col = "cost",
        fill_value = NA
      )
    }
  }

  data.table::setnames(access, c(group_id, "cost"), c("id", travel_cost))
  data.table::setcolorder(
    access,
    c("id", setdiff(groups, group_id), travel_cost)
  )
  data.table::setorderv(access, c("id", setdiff(groups, group_id)))

  if (exists("original_class")) class(access) <- original_class

  return(access[])
}
