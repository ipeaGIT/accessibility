#' Linear decay function
#'
#' Returns a linear weighting function to be used inside accessibility
#' calculating functions.
#' @template description_generic_cost
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff until
#'   which the weighting factor decays linearly. From this point onward the
#'   weight is equal to 0.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examples
#' weighting_function <- decay_linear(cutoff = 30)
#'
#' weighting_function(20)
#'
#' weighting_function(35)
#'
#' @export
decay_linear <- function(cutoff) {
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  weighting_function <- function(travel_cost) {
    weights <- 1 - travel_cost / cutoff
    weights[weights < 0] <- 0
    return(weights)
  }

  return(weighting_function)
}
