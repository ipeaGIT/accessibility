#' Palma ratio
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
#' palma <- palma_ratio(
#'   access,
#'   sociodemographic_data = land_use_data,
#'   opportunity = "jobs",
#'   population = "population",
#'   income = "income_per_capita"
#' )
#' palma
palma_ratio <- function(accessibility_data,
                        sociodemographic_data,
                        opportunity,
                        population,
                        income,
                        group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  checkmate::assert_string(income)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)
  assert_sociodemographic_data(sociodemographic_data, population, income)

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
  merge_by_reference(data, sociodemographic_data, income, left_df_idcol = "id")

  warn_extra_access_cols(accessibility_data, opportunity, group_by)

  income_quantiles <- Hmisc::wtd.quantile(
    sociodemographic_data[[income]],
    weights = sociodemographic_data[[population]],
    probs = c(0.4, 0.9)
  )

  .income_colname <- income
  data[
    get(.income_colname) <= income_quantiles["40%"],
    .palma_group := "poorest"
  ]
  data[
    get(.income_colname) > income_quantiles["90%"],
    .palma_group := "wealthiest"
  ]

  # actually we have to calculate grouping by group_by, then join the datasets

  .opportunity_colname <- opportunity
  wealthiest_avg_access <- data[
    .palma_group == "wealthiest",
    .(avg_access = weighted.mean(get(.opportunity_colname), w = population)),
    by = group_by
  ]
  poorest_avg_access <- data[
    .palma_group == "poorest",
    .(avg_access = weighted.mean(get(.opportunity_colname), w = population)),
    by = group_by
  ]

  if (identical(group_by, character(0))) {
    palma <- data.table::data.table(
      palma_ratio = wealthiest_avg_access$avg_access /
        poorest_avg_access$avg_access
    )
  } else {
    palma <- wealthiest_avg_access[
      poorest_avg_access,
      on = group_by,
      poorest_avg_access := i.avg_access
    ]

    palma[, palma_ratio := avg_access / poorest_avg_access]
    palma[, c("avg_access", "poorest_avg_access") := NULL]
  }

  if (exists("original_class")) class(palma) <- original_class

  return(palma[])
}
