#' @title Binary (aka step) decay function
#'
#' @description
#' Returns a step a number of impedance function to be used inside `accessibility`
#' functions.
#'
#' @param cutoff A `numeric` value. A number indicating the max cutoff point of
#'        travel cost.
#'
#' @return A `function`
#'
#' @family Impedance functions
#'
#' @details
#' Binary (aka step) function commonly used in cumulative opportunity measures.
#'
#'\deqn{f(t_{ij})\leq T\begin{Bmatrix}
#'      1 & for & t_{ij}\leq T \\
#'      0 & for & t_{ij}>  T
#'      \end{Bmatrix}
#'      }
#'
#'Where:
#'- *tij* is the travel cost between origin * and destination *j*.
#'- *T* is the `cutoff` of maximu travel cost.
#'
#' @examples
#' library(accessibility)
#'
#'# Create a binary impedance function
#'impedance <- decay_binary(cutoff = 30)
#'
#'impedance(t_ij = 20)
#'
#'impedance(t_ij = 35)
#'
#' @export
decay_binary <- function(cutoff) {

  # check inputs ------------------------------------------------------------
  checkmate::assert_number(cutoff, null.ok = FALSE, lower = 0)

  # decay function ------------------------------------------------------------
  impedance <- function(t_ij) {
    f <- data.table::fifelse(t_ij <= cutoff, 1, 0)
    return(f)
  }

  return(impedance)

}
