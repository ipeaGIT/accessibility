#' @title BFCA balanced floating catchment area
#'
#' @description
#' Calculates accessibility levels using the balanced floating catchment area
#' (BFCA) measure proposed by Paez et al. (2019). The BFCA metric calculate
#' accessibility accounting for the competition of resources and simultaneously
#' correcting for issues of inflation of demand and service levels that are
#' present in other accessibility metrics in the floating catchment area family.
#'
#' @template input_data
#' @template arguments_fca
#' @template decay_function
#' @template opportunity_col
#' @template travel_cost_col
#'
#' @return A `numeric` estimate of accessibility.
#'
#' @family Floating catchment area
#' @examples
#' library(accessibility)
#'
#' # load a travel time matrix data in long format
#' data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
#' ttm <- readRDS(data_path)
#'
#'# BFCA with a step decay function
#'df <- fca_bfca(data = ttm,
#'        orig_col = 'from_id',
#'        dest_col = 'to_id',
#'        opportunity_col = 'jobs',
#'        population_col = 'population',
#'        decay_function = decay_binary(cutoff = 50),
#'        travel_cost_col = 'travel_time'
#'        )
#'
#'head(df)
#'
#'df2 <- fca_bfca(data = ttm,
#'         orig_col = 'from_id',
#'         dest_col = 'to_id',
#'         opportunity_col = 'jobs',
#'         population_col = 'population',
#'         decay_function = decay_exponential(decay_value = 0.5),
#'         travel_cost_col = 'travel_time'
#'         )
#'
#'head(df2)
#'
fca_bfca <- function(data,
                     orig_col,
                     dest_col,
                     population_col,
                     opportunity_col,
                     decay_function,
                     travel_cost_col='travel_time'){

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'


  # check inputs ------------------------------------------------------------
  checkmate::assert_function(decay_function)


  # calculate access -----------------------------------------------------------

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'

  # calculate impedance
  data[, impedance := decay_function(t_ij = get(travel_cost_col)),]

  # calculate balanced impedance i (normalizing impedance by origin)
  data[, balanced_impedance_i := impedance/sum(impedance),
       by= c(orig_col)]


  # calculate balanced impedance j (normalizing impedance by destination)
  data[, balanced_impedance_j := impedance/sum(impedance),
       by= c(dest_col)]


  ## Step 1 - allocate the demand to each destination proportionally to weight i
  data[, pop_served := sum( get(population_col) * balanced_impedance_i, na.rm = TRUE),
       by= c(dest_col)]

  ## Step 2 - calculate provider-to-population ration (ppr) at each destination
  # The level of service of an area is the number of opportunities/resources in the area, divided by the population it serves:
  # level of service == provider-to-population ratio (PPR)
  data[, ppr := data.table::first( get(opportunity_col)) / pop_served,
       by= c(dest_col)]


  ## Step 3 - reaportion ppr at each origin proportionally to weight j
  bfca <- data[, .(access_bfca = sum(ppr * balanced_impedance_j, na.rm=T)),
               by= c(orig_col)]

  # delete temp columns
  data[, c('impedance', 'balanced_impedance_i', 'balanced_impedance_j', 'pop_served', 'ppr') := NULL]

  return(bfca)
}
