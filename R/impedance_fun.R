#' @title General impedance functions
#'
#' @description
#' Provides a number of impedance functions to be used inside `accessibility`
#' functions.
#'
#' @param t_ij A number indicating the travel cost (time or money) between
#'             origin (i) and destination (j).
#' @param decay_function A string. Which decay function to use when calculating
#'            accessibility. One of step, exponential, fixed_exponential, linear
#'            or logistic. Please see the details to understand how each
#'            alternative works and how they relate to the `cutoffs` and
#'            `decay_value` parameters.
#' @param cutoff A numeric vector. This parameter has different effects for each
#'               decay function: it indicates the cutoff times in minutes when
#'               calculating cumulative opportunities accessibility with the
#'               `step` function...
#' @param decay_value A number. Extra parameter to be passed to the selected
#'               `decay_function`. Has no effects when `decay_function` is either
#'               `step` or `exponential`.
#'
#' @return A `numeric` impedance factor
#'
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'# Active accessibility: number of schools accessible from each origin
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_col = 'schools',
#'                             cutoff = 30,
#'                             by_col = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_col = 'population',
#'                             cutoff = 30,
#'                             by_col = 'to_id')
#'head(df)
#' @family Impedance functions
#' @export
impedance_fun <- function(t_ij, decay_function, cutoff=NULL, decay_value=NULL){

  # check inputs ------------------------------------------------------------
  checkmate::assert_string(decay_function, null.ok = FALSE)
  checkmate::assert_numeric(t_ij, null.ok = FALSE, lower = 0, finite = TRUE)
  checkmate::assert_number(cutoff, null.ok = TRUE, lower = 0)
  checkmate::assert_number(decay_value, null.ok = TRUE, lower = 0, finite = TRUE)

  decay_options <- c('negative_exponential', 'inverse_power', 'modified_gaussian', 'linear', 'step')
  if (! decay_function %in% decay_options){stop("Parameter 'decay_function' must be one of the following: ", paste0(decay_options, collapse = ", "))}

  # impedance functions ------------------------------------------------------------

  if(decay_function=='negative_exponential'){
    if (is.null(decay_value)) {stop("'decay_value' cannot be NULL with this decay_function")}
    impedance <- function(t_ij, cutoff, decay_value){ exp(-decay_value * t_ij) }
    }

  if(decay_function=='inverse_power'){
    if (is.null(decay_value)) {stop("'decay_value' cannot be NULL with this decay_function")}
    impedance <- function(t_ij, cutoff, decay_value){ data.table::fifelse(t_ij < 1, 1, t_ij^-decay_value) }
  }

  if(decay_function=='modified_gaussian'){
    if (is.null(decay_value)) {stop("'decay_value' cannot be NULL with this decay_function")}
    impedance <- function(t_ij, cutoff, decay_value){ exp(-t_ij^2/decay_value) }
  }

  if(decay_function=='step'){
    if (is.null(cutoff)) {stop("'cutoff' cannot be NULL with this decay_function")}
    impedance <- function(t_ij, cutoff, decay_value){ data.table::fifelse(t_ij <= cutoff, 1, 0) }
  }

  if(decay_function=='linear'){
    if (is.null(cutoff)) {stop("'cutoff' cannot be NULL with this decay_function")}
    impedance <- function(t_ij, cutoff, decay_value){ data.table::fifelse(t_ij <= cutoff, (1-t_ij/cutoff), 0) }
  }

  # get impedance factor
  impedance_factor <- impedance(t_ij, cutoff, decay_value)

  return(impedance_factor)
}

