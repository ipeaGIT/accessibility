#' @title Inverse power decay function
#'
#' @description
#' Returns an inverse power impedance function to be used inside
#' `accessibility` functions.
#'
#' @param decay_value A `numeric` value.
#'
#' @return A `function` that converts travel time cost t_id into an impedance factor.
#'
#' @family Impedance functions
#'
#' @examples
#' library(accessibility)
#'
#'# Create an inverse power impedance function
#'impedance <- decay_power(decay_value = 0.1)
#'
#'impedance(t_ij = 20)
#'impedance(t_ij = 25)
#'impedance(t_ij = 35)
#'
#' @export
decay_power <- function(decay_value) {

  # check inputs ------------------------------------------------------------
  checkmate::assert_number(decay_value, null.ok = FALSE, lower = 0, finite = TRUE)

  # decay function ------------------------------------------------------------
  impedance <- function(t_ij) {
    f <- data.table::fifelse(t_ij < 1, 1, t_ij^-decay_value)
    return(f)
  }

  return(impedance)

}
