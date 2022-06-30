#' Binary (a.k.a. step) decay function
#'
#' Returns a binary impedance function (frequently used to calculate cumulative
#' opportunities measures) to be used inside accessibility calculating
#' functions.
#' @template description_generic_cost
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff.
#'
#' @template return_decay_function
#'
#' @family Impedance functions
#'
#' @examples
#' impedance <- decay_binary(cutoff = 30)
#'
#' impedance(t_ij = 20)
#'
#' impedance(t_ij = 35)
#'
#' @export
decay_binary <- function(cutoff) {
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  impedance <- function(t_ij) {
    impedance_value <- as.integer(t_ij <= cutoff)
    return(impedance_value)
  }

  return(impedance)
}
