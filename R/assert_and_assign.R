#' @keywords internal
assert_group_by <- function(group_by) {
  checkmate::assert_character(group_by, any.missing = FALSE, unique = TRUE)

  checkmate::assert_names(group_by, disjunct.from = c("from_id", "to_id"))

  return(invisible(TRUE))
}


#' @keywords internal
assert_travel_matrix <- function(travel_matrix, travel_cost, group_by) {
  travel_matrix_req_names <- c("from_id", "to_id", travel_cost, group_by)
  checkmate::assert_data_frame(travel_matrix)
  checkmate::assert_names(
    names(travel_matrix),
    must.include = travel_matrix_req_names,
    .var.name = "travel_matrix"
  )

  return(invisible(TRUE))
}


#' @keywords internal
assert_land_use_data <- function(land_use_data, opportunity, demand = NULL) {
  land_use_data_req_names <- c("id", opportunity)
  if (!is.null(demand)) {
    land_use_data_req_names <- c(land_use_data_req_names, demand)
  }

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


#' @keywords internal
assert_decay_function <- function(decay_function) {
  checkmate::assert_function(decay_function)

  result <- tryCatch(
    decay_function(1:100),
    error = function(cnd) cnd
  )

  if (inherits(result, "error")) {
    stop(
      "Assertion on 'decay_function' failed: Must be a function that ",
      "takes a numeric vector as input. Instead, failed with message: ",
      "'", result$message, "'",
      call. = FALSE
    )
  }

  if (!is.numeric(result) || length(result) != length(1:100)) {
    stop(
      "Assertion on 'decay_function' failed: Must be a function that takes ",
      "a numeric vector as input and returns a numeric vector as output, with ",
      "the same length of input. Instead, with input 1:100 it returned: ",
      "{", paste0(result, collapse = ","), "}",
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}


#' @keywords internal
assert_interval_increment <- function(interval_increment) {
  checkmate::assert_number(interval_increment, finite = TRUE)

  if (interval_increment <= 0) {
    stop(
      "Assertion on 'interval_increment' failed: ",
      "Value must be greater than 0."
    )
  }

  return(invisible(TRUE))
}
