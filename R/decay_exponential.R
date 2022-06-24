#' @title Negative exponential decay function
#'
#' @description
#' Returns a negative exponential impedance function to be used inside
#' `accessibility` functions.
#'
#' @param decay_value A `numeric` value.
#'
#' @return A `function` that converts travel time cost t_id into an impedance factor.
#'
#' @family Impedance functions
#'
#' @details
#' Negative exponential.
#'
#'\deqn{ f(t_{ij}) = e^{(-\beta t_{ij})} }
#'
#'Where:
#'- \eqn{t_{ij}} is the travel cost between origin *i* and destination *j*.
#'- \eqn{\beta} is the `decay_value` that tells the speed of decay.
#'
#' @examples
#' library(accessibility)
#'
#'# Create an exponential impedance function
#'impedance <- decay_exponential(decay_value = 0.1)
#'
#'impedance(t_ij = 20)
#'impedance(t_ij = 25)
#'impedance(t_ij = 35)
#'
#' @export
decay_exponential <- function(decay_value) {

  # check inputs ------------------------------------------------------------
  checkmate::assert_number(decay_value, null.ok = FALSE, lower = 0, finite = TRUE)

  # decay function ------------------------------------------------------------
  impedance <- function(t_ij) {
    f <- exp(-decay_value * t_ij)
    return(f)
  }

  return(impedance)

}
