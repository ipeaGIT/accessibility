#' Floating catchment area accessibility
#'
#' Calculates accessibility accounting for the competition of resources using
#' one of the multiple accessibility metrics in the floating catchment area
#' family. The function currently includes `2SFCA` 2-Step Floating Catchment
#' Area \insertCite{luo2003measures}{accessibility}, and `BFCA` Balanced
#' Floating Catchment Area \insertCite{paez2019demand}{accessibility}.
#' Accessibility can be calculated using multiple alternative decay functions
#' passed through the `decay_function` parameter.
#'
#' @template input_data
#' @param fca_metric A `string` indicating which floating catchment area
#'   accessibility measure to use. Options available: `"2SFCA"` and `"BFCA"`.
#'   More info in the details.
#' @template arguments_fca
#' @template decay_function
#' @template opportunity_col
#' @template travel_cost_col
#'
#' @return A `data.table` object.
#'
#' @details
#' # 2SFCA
#' The 2SFCA measure was the first accessibility metric in the floating
#' catchment area family. It was originally proposed by
#' \insertCite{luo2003measures;textual}{accessibility}.
#'
#' # BFCA
#' The balanced floating catchment area (BFCA) metric calculates accessibility
#' accounting for competition effects while simultaneously correcting for
#' issues of inflation of demand and service levels that are present in
#' previous floating  catchment area measures. BFCA was  was originally
#' proposed by \insertCite{paez2019demand;textual}{accessibility} and named in
#' \insertCite{pereira2021geographic;textual}{accessibility}.
#'
#' @references
#' \insertAllCited{}
#'
#' @family Floating catchment area
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
#'        decay_function = decay_binary(cutoff = 50),
#'        travel_cost_col = 'travel_time'
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
#'        decay_function = decay_exponential(decay_value = 0.5),
#'        travel_cost_col = 'travel_time'
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
                                    decay_function,
                                    travel_cost_col = 'travel_time'){

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
  checkmate::assert_string(travel_cost_col)
  checkmate::assert_names(names(data), must.include = orig_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = dest_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = opportunity_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = population_col, .var.name = "data")
  checkmate::assert_names(names(data), must.include = travel_cost_col, .var.name = "data")

  checkmate::assert_function(decay_function)

  fca_options <- c('2SFCA', 'BFCA')
  if (! fca_metric %in% fca_options){stop("Parameter 'fca_metric' must be one of the following: ", paste0(fca_options, collapse = ", "))}

  # 2SFCA
  if (fca_metric=='2SFCA') {
    access <- fca_2sfca(data = data,
                       orig_col = orig_col,
                       dest_col = dest_col,
                       population_col = population_col,
                       opportunity_col = opportunity_col,
                       decay_function = decay_function,
                       travel_cost_col = travel_cost_col)
    }

  # BFCA
  if (fca_metric=='BFCA') {
    access <- fca_bfca(data = data,
                       orig_col = orig_col,
                       dest_col = dest_col,
                       population_col = population_col,
                       opportunity_col = opportunity_col,
                       decay_function = decay_function,
                       travel_cost_col = travel_cost_col)
    }

  return(access)
}
