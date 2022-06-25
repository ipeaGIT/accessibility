#' @title Floating catchment area accessibility
#'
#' @description
#' Calculates accessibility accounting for the competition of resources using one
#' of the multiple accessibility metrics in the floating catchment area family.
#' The function currently includes `2SFCA` 2-Step Floating Catchment Area and
#' `BFCA` Balanced Floating Catchment Area. Accessibility can be calculated using
#' multiple alternative decay functions passed through the `decay_function`
#'  parameter.
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
#' @details
#' # 2SFCA
#' The 2SFCA measure was the first accessibility metric in the floating
#' catchment area family. It was originally proposed by Luo & Wang (2003).
#'
#' # BFCA
#' The balanced floating catchment area (BFCA) metric calculates accessibility
#' accounting for competition effects while simultaneously correcting for issues
#' of inflation of demand and service levels that are present in previous
#' floating  catchment area measures. BFCA was  was originally proposed by Paez
#' et al. (2019) and named in Pereira et al. (2021).
#'
#' ## References:
#' - Luo, W., & Wang, F. (2003). Measures of spatial accessibility to health
#' care in a GIS environment: synthesis and a case study in the Chicago region.
#' Environment and planning B: planning and design, 30(6), 865-884. \doi{10.1068/b29120}.
#'
#' - Paez, A., Higgins, C. D., & Vivona, S. F. (2019). Demand and level of
#' service inflation in Floating Catchment Area (FCA) methods. Plos one, 14(6),
#' e0218773. \doi{10.1371/journal.pone.0218773}
#'
#' - Pereira, R. H., Braga, C. K. V., Servo, L. M., Serra, B., Amaral, P.,
#' Gouveia, N., & Paez, A. (2021). Geographic access to COVID-19 healthcare in
#' Brazil using a balanced float catchment area approach. Social Science &
#' Medicine, 273. \doi{10.1016/j.socscimed.2021.113773}
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
  #                      decay_function = decay_function,
  #                      travel_cost_col = travel_cost_col
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
