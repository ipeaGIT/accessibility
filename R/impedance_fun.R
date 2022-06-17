#' General impedance functions
#'
#' Provides a numer of impedance functions to be used inside `accessibility` functions.
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
#' @return A `function`
#'
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_poa.csv", package = "accessibility")
#' ttm <- read.csv(data_path)
#'
#'# Active accessibility: number of schools accessible from each origin
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_colname = 'schools',
#'                             cutoff = 30,
#'                             by_colname = 'from_id')
#'head(df)
#'
#'# Passive accessibility: number of people that can reach each destination
#'df <- cumulative_time_cutoff(data = ttm,
#'                             opportunity_colname = 'population',
#'                             cutoff = 30,
#'                             by_colname = 'to_id')
#'head(df)
#' @family Impedance functions
#' @export
impedance_fun <- function(t_ij, decay_function, cutoff=NULL, decay_value=NULL){

  # check inputs ------------------------------------------------------------
  checkmate::test_string(decay_function, null.ok = FALSE)
  checkmate::test_number(cutoff, null.ok = TRUE)
  checkmate::test_number(decay_value, null.ok = TRUE)

  # impedance functions ------------------------------------------------------------

  if(decay_function=='negative_exponential'){
    impedance <- function(t_ij, cutoff, decay_value){ exp(-decay_value * t_ij) }
    }

  if(decay_function=='inverse_power'){
    impedance <- function(t_ij, cutoff, decay_value){ t_ij^-decay_value }
  }

  if(decay_function=='modified_gaussian'){
    impedance <- function(t_ij, cutoff, decay_value){ exp(-t_ij^2/decay_value) }
  }

  if(decay_function=='step'){
    impedance <- function(t_ij, cutoff, decay_value){ t_ij * data.table::fifelse(t_ij <= cutoff, 1, 0) }
  }

  if(decay_function=='linear'){
    impedance <- function(t_ij, cutoff, decay_value){ t_ij * data.table::fifelse(t_ij <= cutoff, (1-t_ij/cutoff), 0) }
  }

  # get impedance factor
  impedance_factor <- impedance(t_ij, cutoff, decay_value)

  return(impedance_factor)
}

