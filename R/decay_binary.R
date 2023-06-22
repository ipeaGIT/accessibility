#' Binary (a.k.a. step) decay function
#'
#' Returns a binary weighting function (frequently used to calculate cumulative
#' opportunities measures) to be used inside accessibility calculating
#' functions.
#' @template description_generic_cost
#'
#' @param cutoff A `numeric` vector. The numbers indicating the travel cost
#'   cutoffs.
#'
#' @template return_decay_function
#'
#' @family decay functions
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' weighting_function <- decay_binary(cutoff = 30)
#'
#' weighting_function(c(20, 35))
#'
#' weighting_function <- decay_binary(cutoff = c(30, 45))
#'
#' weighting_function(c(20, 35))
#'
#' @export
decay_binary <- function(cutoff) {
  checkmate::assert_numeric(
    cutoff,
    lower = 0,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE
  )

  weighting_function <- function(travel_cost) {
    weights_list <- lapply(
      cutoff,
      function(x) {
        weights <- as.integer(travel_cost <= x)
        weights
      }
    )
    names(weights_list) <- cutoff

    return(weights_list)
  }

  return(weighting_function)
}
