#' @keywords internal
assert_cutoff <- function(cutoff, travel_cost) {
  if (length(travel_cost) == 1) {
    checkmate::assert_numeric(
      cutoff,
      lower = 0,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1,
      unique = TRUE
    )
  } else {
    checkmate::assert_list(
      cutoff,
      any.missing = FALSE,
      len = length(travel_cost)
    )

    cutoff_collection <- checkmate::makeAssertCollection()

    for (i in seq.int(1, length(travel_cost))) {
      checkmate::assert_numeric(
        cutoff[[i]],
        lower = 0,
        finite = TRUE,
        any.missing = FALSE,
        min.len = 1,
        unique = TRUE,
        .var.name = paste0("cutoff[[", i, "]]"),
        add = cutoff_collection
      )
    }

    checkmate::reportAssertions(cutoff_collection)
  }

  return(invisible(TRUE))
}


#' @keywords internal
assert_group_by <- function(group_by) {
  checkmate::assert_character(group_by, any.missing = FALSE, unique = TRUE)

  checkmate::assert_names(group_by, disjunct.from = c("from_id", "to_id"))

  return(invisible(TRUE))
}


#' @keywords internal
assert_access_group_by <- function(group_by) {
  checkmate::assert_character(group_by, any.missing = FALSE, unique = TRUE)

  checkmate::assert_names(group_by, disjunct.from = "id")

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
assert_accessibility_data <- function(accessibility_data,
                                      opportunity,
                                      group_by) {
  required_names <- c("id", opportunity, group_by)

  checkmate::assert_data_frame(accessibility_data)
  checkmate::assert_names(
    names(accessibility_data),
    must.include = required_names,
    .var.name = "accessibility_data"
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
assert_sociodemographic_data <- function(sociodemographic_data,
                                         columns) {
  required_names <- c("id", columns)

  checkmate::assert_data_frame(sociodemographic_data)
  checkmate::assert_names(
    names(sociodemographic_data),
    must.include = required_names,
    .var.name = "sociodemographic_data"
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

  # result must be either a numeric vector with same length as input or a list
  # of numeric vectors whose lenghts are the same as input

  is_numeric_like <- function(x) is.numeric(x) || is.integer(x)

  right_numeric <- is_numeric_like(result) && length(result) == 100

  numeric_like_elements <- vapply(result, is_numeric_like, logical(1))
  right_classes <- length(result) > 0 && all(numeric_like_elements)

  elements_length <- vapply(result, length, integer(1))
  right_lengths <- length(result) > 0 && all(elements_length == 100)

  if (!(right_numeric || (is.list(result) && right_classes && right_lengths))) {
    stop(
      "Assertion on 'decay_function' failed: Must be a function that takes ",
      "a numeric vector as input and returns either:\n",
      " - a numeric vector with the same length of input, or;\n",
      " - a list of numeric vectors whose lengths are the same of input.\n",
      "Instead, with input 1:100 it returned: ",
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


#' @keywords internal
assert_and_assign_interval <- function(interval) {
  is_numeric_like <- function(x) is.numeric(x) || is.integer(x)

  build_interval_name <- function(pair) {
    paste0("[", pair[1], ",", pair[2], "]")
  }

  if (is.list(interval)) {
    checkmate::assert_list(
      interval,
      types = "numeric",
      any.missing = FALSE,
      min.len = 1,
      unique = TRUE
    )

    for (i in seq.int(length(interval))) {
      checkmate::assert_numeric(
        interval[[i]],
        lower = 0,
        any.missing = FALSE,
        len = 2,
        unique = TRUE,
        sorted = TRUE,
        finite = TRUE,
        .var.name = paste0("interval[[", i, "]]")
      )
    }

    names(interval) <- vapply(interval, build_interval_name, character(1))
  } else if (is_numeric_like(interval)) {
    checkmate::assert_numeric(
      interval,
      lower = 0,
      any.missing = FALSE,
      len = 2,
      unique = TRUE,
      sorted = TRUE,
      finite = TRUE
    )

    interval <- list(interval)
    names(interval) <- build_interval_name(interval[[1]])
  } else {
    stop(
      "Assertion on 'interval' failed: Must be either:\n",
      " - a numeric vector with 2 elements, indicating the start and end ",
      "points of the travel cost interval;\n",
      " - a list of numeric vectors with 2 elements, each one of them ",
      "indicating the start and end points of a travel cost interval."
    )
  }

  return(interval)
}

