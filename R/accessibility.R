#' accessibility: Transport accessibility metrics
#'
#' Transport accessibility metrics
#'
#' @section Usage:
#' Please check the vignettes for more on the package usage:

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
    "cum_opport"
    )
)
