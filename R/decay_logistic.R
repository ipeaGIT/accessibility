#' Logistic-CDF decay function
#'
#' Returns a logist-cdf weighting function parameterized with the
#' median (inflection point) and standard deviation to be used inside
#' accessibility calculating functions.
#'
#' @template description_generic_cost
#'
#' @param cutoff A `numeric` vector. The median or inflection point
#'   of the logistic CDF in minutes of travel time.
#' @param sd A `numeric` vector with same length as `cutoff`.
#'   The standard deviation in minutes of the logistic-CDF decay function
#'   must be greater than 0 and less than 120.
#'   Values near 0 result approximate binary decay, values near 120
#'   approximate linear decay.
#'
#' @return A `function` that takes a generic travel cost vector (`numeric`) as
#'   an input and returns a vector of weights (`numeric`).
#'
#' @details When using a function created with `decay_logistic()`, the
#'   output is named after the combination of cutoff (`"T"`) and sd (`"s"`)
#'   - e.g. given the cutoff `c(10, 20)` and the sd `c(10, 20)`,
#'   the outputs will be named `"T10;s10"`, `"T20;s20"`.
#'
#' @family decay functions
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' weighting_function <- decay_logistic(
#'   cutoff = seq(10, 120, by = 10),
#'   sd = 10
#' )
#'
#' weighting_function(seq(0, 120, by = 5))
#'
#' weighting_function <- decay_logistic(
#'   c(10, 10, 10, 10, 20, 20, 20, 20),
#'   c(2, 4, 6, 8, 10, 12, 2, 4, 6, 8, 10, 12)
#' )
#'
#' weighting_function(seq(0, 120, by = 5))
#'
#' @export
decay_logistic <- function(cutoff, sd) {
  checkmate::assert_numeric(
    cutoff,
    lower = 0.001,
    finite = TRUE,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    sorted = TRUE
  )
  checkmate::assert_numeric(
    sd,
    lower = 0.001,
    upper = 119.999,
    any.missing = FALSE,
    len = length(cutoff)
  )

  SQRT3 = sqrt(3)
  g = function(travel_cost, med_m, sd_m) {
    1 + exp(((travel_cost - med_m) * pi) / (sd_m * SQRT3))
  }

  weighting_function <- function(travel_cost) {
    sd_list <- mapply(
      meds = cutoff,
      sds = sd,
      FUN = function(meds, sds) {
        vapply(
          travel_cost,
          function(x) g(0, meds, sds) / g(x, meds, sds),
          numeric(1)
        )
      },
      SIMPLIFY = FALSE
    )

    list_names <- sprintf('T%0.0f;s%0.0f', cutoff, sd)
    names(sd_list) <- list_names

    return(sd_list)
  }

  return(weighting_function)
}
