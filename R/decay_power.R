#' Inverse power decay function
#'
#' Returns an inverse power impedance function  to be used inside accessibility
#' calculating functions.
#' @template description_generic_cost
#'
#' @param decay_value A `numeric`. The calibration parameter to be used as the
#'   exponent in the inverse power function.
#'
#' @template return_decay_function
#'
#' @family Impedance functions
#'
#' @examples
#' impedance <- decay_power(decay_value = 0.1)
#'
#' impedance(20)
#'
#' impedance(35)
#'
#' @export
decay_power <- function(decay_value) {
  checkmate::assert_number(decay_value, lower = 0, finite = TRUE)

  impedance <- function(t_ij) {
    impedance_value <- t_ij ^ (-decay_value)
    impedance_value[impedance_value > 1] <- 1
    return(impedance_value)
  }

  return(impedance)
}
