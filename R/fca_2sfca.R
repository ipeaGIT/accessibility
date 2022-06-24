#' @title 2SFCA two-step floating catchment area
#'
#' @description
#' Calculates accessibility levels using the two-step floating catchment area
#' (2SFCA) proposed by Luo and Wang (2003). The 2SFCA metric was the first
#' accessibility metric in the floating catchment area family.
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
#'# 2SFCA with a step decay function
#'df <- fca_2sfca(data = ttm,
#'               orig_col = 'from_id',
#'               dest_col = 'to_id',
#'               opportunity_col = 'jobs',
#'               population_col = 'population',
#'               decay_function = decay_binary(cutoff = 50),
#'               travel_cost_col = 'travel_time'
#'               )
#'head(df)
#'
#'# 2SFCA with an exponential decay function
#'df2 <- fca_2sfca(data = ttm,
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
fca_2sfca <- function(data,
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


  # calculate access -----------------------------------------------------------

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'

  # calculate impedance
  data[, impedance := decay_function(t_ij = get(travel_cost_col)),]


  ## Step 1a - allocate the demand to each destination proportionally to weight i
  data[ impedance > 0,
        pop_served := sum( get(population_col) * impedance, na.rm = TRUE),
        by= c(dest_col)]


  ## Step 1b - calculate provider-to-population ration (ppr) at each destination
  # The level of service of an area is the number of opportunities/resources in the area, divided by the population it serves:
  # level of service == provider-to-population ratio (PPR)
  data[, ppr := data.table::first( get(opportunity_col)) / pop_served,
         by= c(dest_col)]


  ## Step 2 - reaportion ppr at each origin proportionally to weight j
  access_2sfca <- data[ impedance > 0,
                       .(access_2sfca = sum(ppr * impedance, na.rm=T)),
                       by= c(orig_col)]

  # delete temp columns
  data[, c('impedance', 'pop_served', 'ppr') := NULL]

  return(access_2sfca)
}
