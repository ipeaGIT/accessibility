#' @title 2SFCA two-step floating catchment area
#'
#' @description
#' Calculates accessibility levels using the two-step floating catchment area
#' (2SFCA) proposed by Luo and Wang (2003). The 2SFCA metric was the first
#' accessibility metric in the the floating catchment area family.
#'
#' @param data A `data.frame` with a travel time matrix in long format,
#'   containing the at least the columns of origin `from_id`, destination `to_id`,
#'   travel time `travel_time` from origin to destination, and number of
#'   opportunities in destination locations.
#'
#' @param orig_col A `string` with the name of the column of origin ids.
#' @param dest_col A `string` with the name of the column of destination ids.
#' @param population_col A `string` with the name of the column of origin with
#'       population count.
#' @param opportunity_col A `string` with the name of the column of destination
#'        with  the number of opportunities / resources / services.
#' @param decay_function A `fuction` that converts travel cost into and impedance
#'   factor used to weigth opportunities. For convinence, the package currently
#'   includes the following functions: [decay_bineary()], [decay_linear()] and
#'   [decay_exponential()]. See the documentation of each function for more
#'   details.
#'
#' @return A `numeric` estimate of accessibility.
#'
#' @details
#' References:
#' - Luo, W., & Wang, F. (2003). Measures of spatial accessibility to health
#' care in a GIS environment: synthesis and a case study in the Chicago region.
#' Environment and planning B: planning and design, 30(6), 865-884. \doi{10.1068/b29120}.
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
#'               decay_function = decay_linear(cutoff = 50)
#'               )
#'head(df)
#'
#'# 2SFCA with an exponential decay function
#'df2 <- fca_2sfca(data = ttm,
#'         orig_col = 'from_id',
#'         dest_col = 'to_id',
#'         opportunity_col = 'jobs',
#'         population_col = 'population',
#'         decay_function = decay_exponential(decay_value = 0.5)
#'         )
#'
#'head(df2)
#'
#' @export
fca_2sfca <- function(data,
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


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'

  # calculate impedance
  data[, impedance := decay_function(t_ij = travel_time),]


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
