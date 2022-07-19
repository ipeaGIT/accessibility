#' Binary (a.k.a. step) decay function
#'
#' Returns a binary weighting function (frequently used to calculate cumulative
#' opportunities measures) to be used inside accessibility calculating
#' functions.
#' @template description_generic_cost
#'
#' @param cutoff A `numeric`. A number indicating the travel cost cutoff.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examples
#' weighting_function <- decay_binary(cutoff = 30)
#'
#' weighting_function(20)
#'
#' weighting_function(35)
#'
#' @export
decay_binary <- function(cutoff) {
  checkmate::assert_number(cutoff, lower = 0, finite = TRUE)

  weighting_function <- function(travel_cost) {
    weights <- as.integer(travel_cost <= cutoff)
    return(weights)
  }

  return(weighting_function)
}
