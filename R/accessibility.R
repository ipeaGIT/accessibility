#' accessibility: Transport accessibility metrics
#'
#' Transport accessibility metrics
#'
#' @section Usage:
#' Please check the vignettes for more on the package usage:
#' - Introduction to accessibility: calculating accessibility metrics. Run
#' `vignette("accessibility")` or check it on the [website](
#' https://ipeagit.github.io/accessibility/articles/accessibility.html).
#' - Decay functions. Run `vignette("decay_functions", package =
#' "accessibility")` or check it on the [website](
#' https://ipeagit.github.io/accessibility/articles/decay_binary.html).
#'
#' @docType package
#' @name accessibility
#' @aliases accessibility-package
#'
#' @importFrom data.table := .I .SD %chin% .GRP .N
#' @importFrom utils globalVariables
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(
  c(
    ".",
    "from_id",
    "schools",
    "df",
    "median",
    "mean",
    "travel_time",
    "to_id",
    "cum_opport",
    "impedance",
    "balanced_impedance_i",
    "balanced_impedance_j",
    "pop_served",
    "ppr",
    "access"
  )
)




