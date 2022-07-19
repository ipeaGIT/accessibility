#' Negative exponential decay function
#'
#' Returns a negative exponential weighting function to be used inside
#' accessibility calculating functions.
#' @template description_generic_cost
#'
#' @param decay_value A `numeric`. The calibration parameter that, when
#'   multiplied by the travel cost, is used as the exponent of `e` in the
#'   negative exponential function.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examples
#' weighting_function <- decay_exponential(decay_value = 0.1)
#'
#' weighting_function(20)
#'
#' weighting_function(35)
#'
#' @export
decay_exponential <- function(decay_value) {
  checkmate::assert_number(decay_value, lower = 0, finite = TRUE)

  weighting_function <- function(travel_cost) {
    weights <- exp(-decay_value * travel_cost)
    return(weights)
  }

  return(weighting_function)
}
