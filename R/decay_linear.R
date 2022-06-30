#' Linear decay function
#'
#' Returns a linear impedance function to be used inside accessibility
#' calculating functions.
#' @template description_generic_cost
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff until
#'   which the impedance factor decays linearly. From this point onward the
#'   impedance factor is equal to 0.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examples
#' impedance <- decay_linear(cutoff = 30)
#'
#' impedance(20)
#'
#' impedance(35)
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
