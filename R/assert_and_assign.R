#' @keywords internal
assert_and_assign_by_col <- function(by_col) {
  checkmate::assert_string(by_col, null.ok = TRUE)

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

  return(by_col_char)
}


#' @keywords internal
assert_travel_matrix <- function(travel_matrix, travel_cost_col, by_col_char) {
  travel_matrix_req_names <- c("from_id", "to_id", travel_cost_col, by_col_char)
  checkmate::assert_data_frame(travel_matrix)
  checkmate::assert_names(
    names(travel_matrix),
    must.include = travel_matrix_req_names,
    .var.name = "travel_matrix"
  )

  return(invisible(TRUE))
}


#' @keywords internal
assert_land_use_data <- function(land_use_data, opportunity_col) {
  land_use_data_req_names <- c("id", opportunity_col)
  checkmate::assert_data_frame(land_use_data)
  checkmate::assert_names(
    names(land_use_data),
    must.include = land_use_data_req_names,
    .var.name = "land_use_data"
  )

  return(invisible(TRUE))
}


#' @keywords internal
assert_summary_function <- function(summary_function) {
  checkmate::assert_function(summary_function)

  result <- tryCatch(
    summary_function(1:100),
    error = function(cnd) cnd
  )

  if (inherits(result, "error")) {
    stop(
      "Assertion on 'summary_function' failed: Must be a function that ",
      "takes a numeric vector as input. Instead, failed with message: ",
      "'", result$message, "'",
      call. = FALSE
    )
  }

  if (!is.numeric(result) || length(result) != 1) {
    stop(
      "Assertion on 'summary_function' failed: Must be a function that takes ",
      "a numeric vector as input and returns a single numeric as output. ",
      "Instead, returned: ",
      "{", paste0(result, collapse = ","), "}",
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}
