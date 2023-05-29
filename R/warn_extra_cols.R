#' @keywords internal
warn_extra_cols <- function(travel_matrix, travel_cost, group_id, groups) {
  travel_matrix_extra_cols <- setdiff(names(travel_matrix), groups)
  travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, travel_cost)
  if (group_id == "from_id") {
    travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, "to_id")
  } else {
    travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, "from_id")
  }

  if (!identical(travel_matrix_extra_cols, character(0))) {
    warning(
      "Found columns in 'travel_matrix' not listed in either ",
      "'travel_cost' or 'group_by': {",
      paste0("'", travel_matrix_extra_cols, "'", collapse = ","),
      "}.\n",
      "This can result in excessive aggregation when calculating ",
      "accessibility, if these columns serve as group ids in 'travel_matrix'.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' @keywords internal
warn_extra_access_cols <- function(accessibility_data, opportunity, group_id) {
  extra_cols <- setdiff(
    names(accessibility_data),
    c("id", opportunity, group_id)
  )

  if (!identical(extra_cols, character(0))) {
    warning(
      "Found columns in 'accessibility_data' not listed in either ",
      "'opportunity' or 'group_by': {",
      paste0("'", extra_cols, "'", collapse = ","),
      "}.\n",
      "This can result in excessive aggregation when calculating ",
      "inequality measures, if these columns serve as group ids in ",
      "'accessibility_data'.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
