#' @title Floating catchment area accessibilitt
#'
#' @description
#' Calculates accessibility accouting for the competition of resources using one
#' of the multiple accessibility metrics in the floating catchment area family.
#' The function currently includes 2SFCA and BFCA
#'
#' @param data A `data.frame` with a travel time matrix in long format,
#'   containing the at least the columns of origin `from_id`, destination `to_id`,
#'   travel time `travel_time` from origin to destination, and number of
#'   opportunities in destination locations.
#' @param fca_metric A `string` indicating which floating cacthment area
#'        accessibility measure to use. Options avaiblable: `"2SFCA"` and `"BFCA"`.
#' @param orig_col A `string` with the name of the column of origin ids.
#' @param dest_col A `string` with the name of the column of destination ids.
#' @param population_col A `string` with the name of the column of origin with
#'       population count.
#' @param opportunity_col A `string` with the name of the column of destination
#'        with  the number of opportunities / resources / services.
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
#' @return A `numeric` estimate of accessibility.
#'
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'# 2SFCA with a step decay function
#'df <- floating_catchment_area(
#'        data = ttm,
#'        fca_metric = '2SFCA',
#'        orig_col = 'from_id',
#'        dest_col = 'to_id',
#'        opportunity_col = 'jobs',
#'        population_col = 'population',
#'        decay_function = 'step',
#'        cutoff = 30
#'        )
#'head(df)
#'
#'# BFCA with a step decay function
#'df <- floating_catchment_area(
#'        data = ttm,
#'        fca_metric = 'BFCA',
#'        orig_col = 'from_id',
#'        dest_col = 'to_id',
#'        opportunity_col = 'jobs',
#'        population_col = 'population',
#'        decay_function = 'step',
#'        cutoff = 30
#'        )
#'head(df)
#'
#'df2 <- floating_catchment_area(data = ttm,
#'         fca_metric = '2SFCA',
#'         orig_col = 'from_id',
#'         dest_col = 'to_id',
#'         opportunity_col = 'jobs',
#'         population_col = 'population',
#'         decay_function = 'negative_exponential',
#'         decay_value = 0.5
#'         )
#'
#'head(df2)
#'
#' @family Floating catchment area
#' @export
floating_catchment_area <- function(data,
                                    fca_metric,
                                    orig_col,
                                    dest_col,
                                    population_col,
                                    opportunity_col,
                                    decay_function,
                                    cutoff=NULL,
                                    decay_value=NULL){

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'


  # check inputs ------------------------------------------------------------
  checkmate::assert_data_frame(data)

  checkmate::assert_string(orig_col)
  checkmate::assert_string(dest_col)
  checkmate::assert_string(opportunity_col)
  checkmate::assert_string(population_col)
  checkmate::assert_names(names(data), must.include = orig_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = dest_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = opportunity_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = population_col, .var.name = "data")

  checkmate::assert_string(decay_function, null.ok = FALSE)
  checkmate::assert_number(cutoff, null.ok = TRUE, lower = 0)
  checkmate::assert_number(decay_value, null.ok = TRUE, lower = 0, finite = TRUE)

  fca_options <- c('2SFCA', 'BFCA')
  if (! fca_metric %in% fca_options){stop("Parameter 'fca_metric' must be one of the following: ", paste0(fca_options, collapse = ", "))}


  # select FCA metric  ---------------------------------------------------------

  fca_fun <- if(fca_metric=='2SFCA'){ accessibility::fca_2sfca}
  fca_fun <- if(fca_metric=='BFCA'){ accessibility::fca_bfca}

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  access <- fca_fun(data = data,
                    orig_col = orig_col,
                    dest_col = dest_col,
                    population_col = population_col,
                    opportunity_col = opportunity_col,
                    decay_function = decay_function,
                    cutoff = cutoff,
                    decay_value = decay_value)

  return(access)
}
