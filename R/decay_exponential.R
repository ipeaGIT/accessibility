#' Negative exponential decay function
#'
#' Returns a negative exponential impedance function  to be used inside
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
#' impedance <- decay_exponential(decay_value = 0.1)
#'
#' impedance(20)
#'
#' impedance(35)
#'
#' @export
decay_exponential <- function(decay_value) {
  checkmate::assert_number(decay_value, lower = 0, finite = TRUE)

  impedance <- function(travel_cost) {
    impedance_value <- exp(-decay_value * travel_cost)
    return(impedance_value)
  }

  return(impedance)
}
