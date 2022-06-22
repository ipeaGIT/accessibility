#' @title BFCA balanced floating catchment area
#'
#' @description
#' Calculates accessibility levels using the balanced floating catchment area
#' (BFCA) measure proposed by Paez et al. (2019). The BFCS metric calculate
#' accessibility accouting for the competition of resources and simultaneously
#' correcting for issues of inflation of demand and service levels that are
#' present in other accessibility metrics in the floating catchment area family.
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
#' The balanced floating catchment area (BFCA) measure was originally proposed
#' by Paez et al. (2019) and
#' - Paez, A., Higgins, C. D., & Vivona, S. F. (2019). Demand and level of
#' service inflation in Floating Catchment Area (FCA) methods. Plos one, 14(6),
#' e0218773. \doi{10.1371/journal.pone.0218773}
#' - Pereira, R. H., Braga, C. K. V., Servo, L. M., Serra, B., Amaral, P.,
#' Gouveia, N., & Paez, A. (2021). Geographic access to COVID-19 healthcare in
#' Brazil using a balanced float catchment area approach. Social Science &
#' Medicine, 273. \doi{10.1016/j.socscimed.2021.113773}
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
#'        decay_function = decay_linear(cutoff = 50)
#'        )
#'
#'head(df)
#'
#'df2 <- fca_bfca(data = ttm,
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
fca_bfca <- function(data,
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
  checkmate::assert_function(decay_function)


  # calculate access -----------------------------------------------------------
  data.table::setDT(data)

  # orig_col <- 'from_id'
  # dest_col <- 'to_id'
  # opportunity_col <- 'jobs'
  # population_col <- 'population'

  # calculate impedance
  data[, impedance := decay_function(t_ij = travel_time),]

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
