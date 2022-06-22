#' @title Floating catchment area accessibilitt
#'
#' @description
#' Calculates accessibility accouting for the competition of resources using one
#' of the multiple accessibility metrics in the floating catchment area family.
#' The function currently includes 2SFCA and BFCA
#'
#' @template input_data
#' @param fca_metric A `string` indicating which floating cacthment area
#'        accessibility measure to use. Options avaiblable: `"2SFCA"` and `"BFCA"`.
#' @template arguments_fca
#' @template opportunity_col
#' @template decay_function
#'
#' @return A `data.table` object.
#' @family Floating catchment area
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
#'        decay_function = decay_binary(cutoff = 50)
#'        )
#'head(df)
#'
#'# BFCA with an exponential decay function
#'df <- floating_catchment_area(
#'        data = ttm,
#'        fca_metric = 'BFCA',
#'        orig_col = 'from_id',
#'        dest_col = 'to_id',
#'        opportunity_col = 'jobs',
#'        population_col = 'population',
#'        decay_function = decay_exponential(decay_value = 0.5)
#'        )
#'head(df)
#'
#'
#' @export
floating_catchment_area <- function(data,
                                    fca_metric,
                                    orig_col,
                                    dest_col,
                                    population_col,
                                    opportunity_col,
                                    decay_function){

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

  checkmate::assert_function(decay_function)

  fca_options <- c('2SFCA', 'BFCA')
  if (! fca_metric %in% fca_options){stop("Parameter 'fca_metric' must be one of the following: ", paste0(fca_options, collapse = ", "))}


  # # select FCA metric  ---------------------------------------------------------
  ##> THIS throws error in testthat because tests cannot find the 'fca_fun' function
  ##>
  # fca_fun <- if(fca_metric=='2SFCA'){ accessibility::fca_2sfca}
  # fca_fun <- if(fca_metric=='BFCA'){ accessibility::fca_bfca}
  #
  #   access <- fca_fun(data = data,
  #                      orig_col = orig_col,
  #                      dest_col = dest_col,
  #                      population_col = population_col,
  #                      opportunity_col = opportunity_col,
  #                      decay_function = decay_function
  #                      )

  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  # 2SFCA
  if (fca_metric=='2SFCA') {
    access <- fca_2sfca(data = data,
                       orig_col = orig_col,
                       dest_col = dest_col,
                       population_col = population_col,
                       opportunity_col = opportunity_col,
                       decay_function = decay_function)
    }

  # BFCA
  if (fca_metric=='BFCA') {
    access <- fca_bfca(data = data,
                       orig_col = orig_col,
                       dest_col = dest_col,
                       population_col = population_col,
                       opportunity_col = opportunity_col,
                       decay_function = decay_function)
    }

  return(access)
}
