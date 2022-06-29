#' Binary (a.k.a. step) decay function
#'
#' Returns a binary impedance function (frequently used to calculate cumulative
#' opportunities measures) to be used inside accessibility calculating
#' functions. This function is generic over any kind of numeric travel cost,
#' such as distance, time and money.
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff.
#'
#' @return A `function` that takes a generic travel cost (`numeric`) as an input
#'   and returns an impedance factor (`numeric`).
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
