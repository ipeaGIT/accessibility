#' Gini idex
#'
#' Calculates the Gini idex of a given accessibility distribution.
#'
#' @param accessibility_data A data frame. The accessibility levels whose
#'   inequality should be calculated. Must contain the columns `id` and any
#'   others specified in `opportunity`.
#' @param opportunity A string. The name of the column in `accessibility_data`
#'   with the accessibility levels to be considerend when calculating inequality
#'   levels.
#' @param sociodemographic_data A data frame. The distribution of
#'   sociodemographic characteristics of the population in the study area cells.
#'   Must contain the columns `id` and any others specified in `population` and
#'   `income`.
#' @param population A string. The name of the column in `sociodemographic_data`
#'   with the number of people in each cell. Used to weigh accessibility levels
#'   when calculating inequality.
#'
#' @return A numeric value with the Gini index estimate for the study area.
#'
#' @family Inequality
#'
#' @examples
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' access <- cumulative_cutoff(
#'   travel_matrix = travel_matrix,
#'   land_use_data = land_use_data,
#'   cutoff = 30,
#'   opportunity = "jobs",
#'   travel_cost = "travel_time"
#' )
#'
#' gini <- gini_index(
#'   accessibility_data = access,
#'   sociodemographic_data = land_use_data,
#'   opportunity = "jobs",
#'   population = "population"
#' )
#' gini
#'
#' @export
gini_index <- function(accessibility_data,
                 sociodemographic_data,
                 opportunity,
                 population){

  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  # assert_access_group_by(group_by)

  # assert_accessibility_data(accessibility_data, opportunity, group_by)

  ### this should be flexible to input the columns we want. In this case, for
  ### example, we do not require an income column
  # assert_sociodemographic_data(sociodemographic_data, population)


  if (!inherits(accessibility_data, "data.table")) {
    original_class <- class(accessibility_data)
    data <- data.table::as.data.table(accessibility_data)
  } else {
    data <- data.table::copy(accessibility_data)
  }

  if (!inherits(sociodemographic_data, "data.table")) {
    sociodemographic_data <- data.table::as.data.table(sociodemographic_data)
  }

  merge_by_reference(
    data,
    sociodemographic_data,
    population,
    left_df_idcol = "id"
  )

  # warn_extra_access_cols(accessibility_data, opportunity, group_by)

  .opportunity_colname <- opportunity
  .population_colname <- population


  data <- data[ get(.population_colname) > 0, ]
  data <- data[order(get(.opportunity_colname))]


  x_o <- data[, get(.opportunity_colname)]
  w_o <- data[, get(.population_colname)]

  x_cum = c(0, cumsum(as.numeric(x_o * w_o)/sum(x_o * w_o)))
  w_cum = c(0, cumsum(as.numeric(w_o)/sum(w_o)))
  # data[, x_cum := cumsum(as.numeric(get(.opportunity_colname) * get(.population_colname))/
  #                          sum(get(.opportunity_colname) * get(.population_colname)))]
  # data[, w_cum := cumsum(as.numeric(get(.population_colname))/sum(get(.population_colname)))]

  b = x_cum[-length(x_cum)]
  B = x_cum[-1]
  h = diff(w_cum)

  area_under_lorenz = sum( ((B + b)*h)/2 )

  gini <- 1 - 2*area_under_lorenz
  return(gini)
}
