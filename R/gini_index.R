#' Gini index
#'
#' Calculates the Gini index of a given accessibility distribution.
#'
#' @param accessibility_data A data frame. The accessibility levels whose
#'   inequality should be calculated. Must contain the columns `id` and any
#'   others specified in `opportunity`.
#' @param sociodemographic_data A data frame. The distribution of
#'   sociodemographic characteristics of the population in the study area cells.
#'   Must contain the columns `id` and any others specified in `population` and
#'   `income`.
#' @param opportunity A string. The name of the column in `accessibility_data`
#'   with the accessibility levels to be considered when calculating inequality
#'   levels.
#' @param population A string. The name of the column in `sociodemographic_data`
#'   with the number of people in each cell. Used to weigh accessibility levels
#'   when calculating inequality.
#' @param group_by A `character` vector. When not `character(0)` (the default),
#'   indicates the `accessibility_data` columns that should be used to group the
#'   inequality estimates by. For example, if `accessibility_data` includes a
#'   `departure_time` column that specifies the departure time that each entry
#'   refers to, passing `"departure_time"` to this parameter results in
#'   inequality estimates grouped by departure time.
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
                       population,
                       group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)
  assert_sociodemographic_data(sociodemographic_data, population)

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

  warn_extra_access_cols(accessibility_data, opportunity, group_by)

  .opp_colname <- opportunity
  .pop_colname <- population

  data.table::setorderv(data, cols = c(opportunity, group_by))

  data[
    ,
    .prop_pop := get(.pop_colname) / sum(get(.pop_colname)),
    by = group_by
  ]
  data[
    ,
    .cum_total_access := cumsum(
      as.numeric(get(.pop_colname) * get(.opp_colname))
    ) / sum(get(.pop_colname) * get(.opp_colname)),
    by = group_by
  ]

  # to calculate the area under the lorenz curve we simply have to calculate the
  # area of the trapezoids formed by the population on the x axis and the
  # cumulative total access in the y axis.
  # since the lorenz curve starts from the (0,0) point, we have to "add" a 0
  # value to to the beginning of the cumulative total access distribution
  # (otherwise we would have a trapezoid in the first segment, whereas it should
  # always be a triangle)

  data[
    ,
    .small_edge := data.table::shift(.cum_total_access, fill = 0),
    by = group_by
  ]
  data[, .big_edge := .cum_total_access]

  data[, .area_under_curve := ((.small_edge + .big_edge) * .prop_pop) / 2]

  gini <- data[, .(gini_index = 1 - 2 * sum(.area_under_curve)), by = group_by]

  return(gini)
}
