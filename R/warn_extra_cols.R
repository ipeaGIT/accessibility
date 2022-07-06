#' @keywords internal
warn_extra_cols <- function(travel_matrix, travel_cost_col, group_id, groups) {
  travel_matrix_extra_cols <- setdiff(names(travel_matrix), groups)
  travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, travel_cost_col)
  if (group_id == "from_id") {
    travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, "to_id")
  } else {
    travel_matrix_extra_cols <- setdiff(travel_matrix_extra_cols, "from_id")
  }

  if (!identical(travel_matrix_extra_cols, character(0))) {
    warning(
      "Found columns in 'travel_matrix' not listed in either ",
      "'travel_cost_col' or 'by_col': {",
      paste0("'", travel_matrix_extra_cols, "'", collapse = ","),
      "}.\n",
      "This can result in excessive aggregation when calculating ",
      "accessibility, if these columns serve as ids in 'travel_matrix'.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
