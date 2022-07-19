#' Inverse power decay function
#'
#' Returns an inverse power weighting function to be used inside accessibility
#' calculating functions.
#' @template description_generic_cost
#'
#' @param decay_value A `numeric`. The calibration parameter to be used as the
#'   exponent in the inverse power function.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examples
#' weighting_function <- decay_power(decay_value = 0.1)
#'
#' weighting_function(20)
#'
#' weighting_function(35)
#'
#' @export
decay_power <- function(decay_value) {
  checkmate::assert_number(decay_value, lower = 0, finite = TRUE)

  weighting_function <- function(travel_cost) {
    weights <- travel_cost ^ (-decay_value)
    weights[weights > 1] <- 1
    return(weights)
  }

  return(weighting_function)
}
