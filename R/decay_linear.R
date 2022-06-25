#' @title Linear decay function
#'
#' @description
#' Returns a linear impedance function to be used inside `accessibility`
#' functions.
#'
#' @param cutoff A `numeric` value. A number indicating the max cutoff point of
#'        travel cost.
#'
#' @return A `function` that converts travel time cost t_id into an impedance factor.
#'
#' @family Impedance functions
#'
#' @examples
#' library(accessibility)
#'
#'# Create a linear impedance function
#'impedance <- decay_linear(cutoff = 30)
#'
#'impedance(t_ij = 20)
#'impedance(t_ij = 25)
#'impedance(t_ij = 35)
#'
#' @export
decay_linear <- function(cutoff) {

  # check inputs ------------------------------------------------------------
  checkmate::assert_number(cutoff, null.ok = FALSE, lower = 0)

  # decay function ------------------------------------------------------------
  impedance <- function(t_ij) {
    f <- data.table::fifelse(t_ij <= cutoff, (1-t_ij/cutoff), 0)
    return(f)
  }

  return(impedance)

}
