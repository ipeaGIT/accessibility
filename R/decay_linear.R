#' Linear decay function
#'
#' Returns a linear impedance function to be used inside accessibility
#' calculating functions. This function is generic over any kind of numeric
#' travel cost, such as distance, time and money.
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff until
#'   which the impedance factor decays linearly. From this point onward the
#'   impedance factor is equal to 0.
#'
#' @return A `function` that takes a generic travel cost (`numeric`) as an input
#'   and returns an impedance factor (`numeric`).
#'
#' @family Impedance functions
#'
#' @examples
#' impedance <- decay_linear(cutoff = 30)
#'
#' impedance(t_ij = 20)
#' impedance(t_ij = 25)
#' impedance(t_ij = 35)
#'
#' @export
decay_linear <- function(cutoff) {
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  impedance <- function(t_ij) {
    impedance_value <- 1 - t_ij / cutoff
    impedance_value[impedance_value < 0] <- 0
    return(impedance_value)
  }

  return(impedance)
}
