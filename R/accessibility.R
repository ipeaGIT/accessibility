#' accessibility: Transport accessibility measures
#'
#' A set of fast and convenient functions to calculate multiple transport
#' accessibility measures. Given a pre-computed travel cost matrix and a land
#' use dataset (containing the location of jobs, healthcare and population, for
#' example), the package allows one to calculate active and passive
#' accessibility levels using multiple accessibility measures, such as:
#' cumulative opportunities (using either travel cost cutoffs or intervals),
#' minimum travel cost to closest N number of activities, gravity-based (with
#' different decay functions) and different floating catchment area methods.
#'
#' @section Usage:
#' Please check the vignettes for more on the package usage:
#' - Introduction to accessibility: calculating accessibility measures. Run
#' `vignette("accessibility")` or check it on the [website](
#' https://ipeagit.github.io/accessibility/articles/accessibility.html).
#' - Decay functions. Run `vignette("decay_functions", package =
#' "accessibility")` or check it on the [website](
#' https://ipeagit.github.io/accessibility/articles/decay_functions.html).
#'
#' @docType package
#' @name accessibility
#' @aliases accessibility-package
#'
#' @importFrom data.table := .I .SD %chin% .GRP .N fifelse
#' @importFrom utils globalVariables
#' @importFrom Rdpack reprompt
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(
  c(
    ".",
    "from_id",
    "schools",
    "df",
    "travel_time",
    "to_id",
    "cum_opport",
    "impedance",
    "pop_served",
    "ppr",
    "access",
    "min_cost",
    "destination",
    "opp_weight",
    "balanced_opp_weight_i",
    "balanced_opp_weight_j",
    "balanced_pop_served",
    "balanced_ppr",
    "..opportunity",
    "demand_bal_fac",
    "impedance_bal_fac",
    "combined_bal_fac",
    "decay_function_arg",
    "cost",
    "did_balance",
    "avg_access",
    "i.avg_access",
    ".palma_group",
    "poorest_avg_access",
    "p",
    "FGT0",
    "FGT1",
    "FGT2",
    ".area_under_curve",
    ".big_edge",
    ".cum_total_access",
    ".prop_pop",
    ".small_edge",
    ".fgt0",
    ".fgt1",
    ".fgt2",
    ".norm_opp_shortfall"
  )
)




