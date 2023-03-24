#' Stepped decay function
#'
#' Returns a stepped weighting function to be used inside accessibility
#' calculating functions.
#' @template description_generic_cost
#'
#' @param steps A `numeric` vector or a list of `numeric` vectors. The travel
#'   cost steps, in ascending order. Please do not include travel cost 0 as a
#'   step: this is already handled by the function.
#' @param weights A `numeric` vector with same length as `steps` or a list of
#'   `numeric` vectors whose lenghts are equal to the lengths of the elements of
#'   same index in `steps`. The values, between 0 and 1, that the function
#'   assumes at each step. Please do not include weight 1 as the first value:
#'   this is already handled by the function. The function considers the steps'
#'   intervals "open on the right", meaning that the function assumes the step
#'   value at the actual step, not after it. Please see the illustrative
#'   examples for effects of this assumption on the results.
#'
#' @return A `function` that takes a generic travel cost vector (`numeric`) as
#'   an input and returns a vector of weights (`numeric`).
#'
#' @family decay functions
#'
#' @examples
#' weighting_function <- decay_stepped(
#'   c(10, 20, 30, 40),
#'   weights = c(0.75, 0.5, 0.25, 0)
#' )
#'
#' weighting_function(c(5, 25, 35, 45))
#'
#' # intervals are open on the right, so the values change exactly at each step
#' weighting_function(c(0, 10, 20, 30, 40))
#'
#' @export
decay_stepped <- function(steps, weights) {
  assert_steps(steps)
  assert_weights(weights, steps)

  steps <- c(0, steps)
  weights <- c(1, weights)

  weighting_function <- function(travel_cost) {
    weights <- vapply(
      travel_cost,
      function(x) weights[max(which(x >= steps))],
      numeric(1)
    )
    return(weights)
  }

  return(weighting_function)
}


assert_steps <- function(steps) {
  is_numeric_like <- function(x) is.numeric(x) || is.integer(x)

  if (!(is_numeric_like(steps) || is.list(steps))) {
    stop(
      "Assertion on 'steps' failed: Must be either a numeric vector or a list ",
      "of numeric vectors."
    )
  }

  if (is_numeric_like(steps)) {
    checkmate::assert_numeric(
      steps,
      lower = 1,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1,
      unique = TRUE,
      sorted = TRUE
    )
  } else {
    for (i in seq.int(length(steps))) {
      checkmate::assert_numeric(
        steps[[i]],
        lower = 1,
        finite = TRUE,
        any.missing = FALSE,
        min.len = 1,
        unique = TRUE,
        sorted = TRUE,
        .var.name = paste0("steps[[", i, "]]")
      )
    }
  }

  return(invisible(TRUE))
}


assert_weights <- function(weights, steps) {
  is_numeric_like <- function(x) is.numeric(x) || is.integer(x)

  if (is_numeric_like(steps)) {
    checkmate::assert_numeric(
      weights,
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      len = length(steps)
    )
  } else {
    checkmate::assert_list(
      weights,
      types = "numeric",
      len = length(steps)
    )

    for (i in seq.int(length(weights))) {
      checkmate::assert_numeric(
        weights[[i]],
        lower = 0,
        upper = 1,
        any.missing = FALSE,
        len = length(steps[[i]]),
        .var.name = paste0("weights[[", i, "]]")
      )
    }
  }

  return(invisible(TRUE))
}
