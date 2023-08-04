#' Theil Index
#'
#' Theil Index mannn
#'
#' @template accessibility_data
#' @template sociodem_data_without_income
#' @template opportunity_access
#' @template population
#' @param socioeconomic_groups A string. The name of the column in
#'   `sociodemographic_data` whose values identify the socioeconomic groups that
#'   should be used to calculate the between- and within-groups inequality
#'   levels.
#' @template group_by_access
#'
#' @template return_inequality
#'
#' @family inequality
#'
#' @references
#' \insertAllCited{}
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' data_dir <- system.file("extdata", package = "accessibility")
#' travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
#' land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
#'
#' access <- cumulative_cutoff(
#'   travel_matrix,
#'   land_use_data,
#'   cutoff = 30,
#'   opportunity = "jobs",
#'   travel_cost = "travel_time"
#' )
#'
#' ti <- theil_index(
#'   access,
#'   sociodemographic_data = land_use_data,
#'   opportunity = "jobs",
#'   population = "population",
#'   socioeconomic_groups = "income_decile"
#' )
#' ti
#'
#' @export
theil_index <- function(accessibility_data,
                        sociodemographic_data,
                        opportunity,
                        population,
                        socioeconomic_groups,
                        group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  checkmate::assert_string(socioeconomic_groups)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)
  assert_sociodemographic_data(
    sociodemographic_data,
    accessibility_data,
    population = population,
    extra_cols = socioeconomic_groups
  )

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
  merge_by_reference(
    data,
    sociodemographic_data,
    socioeconomic_groups,
    left_df_idcol = "id"
  )

  warn_extra_access_cols(accessibility_data, opportunity, group_by)

  .opp_colname <- opportunity
  .groups <- group_by
  .socioecon_groups <- c(group_by, socioeconomic_groups)

  # we have to filter by opportunity because opportunity = 0 results in
  # theil_t being NaN (because log(0) is NaN)

  data[
    ,
    avg_access := stats::weighted.mean(
      get(..opportunity),
      w = get(..population)
    ),
    by = .groups
  ]

  theil_index <- data[
    get(.opp_colname) > 0,
    .(theil_t = theil_t(get(..opportunity), avg_access, get(..population))),
    by = .groups
  ]

  # within group

  data[
    ,
    group_avg_access := stats::weighted.mean(
      get(..opportunity),
      w = get(..population)
    ),
    by = .socioecon_groups
  ]

  within_group <- data[
    get(.opp_colname) > 0,
    .(
      group_theil_t = theil_t(
        get(..opportunity),
        group_avg_access,
        get(..population)
      ),
      total_access = as.numeric(sum(get(..opportunity) * get(..population)))
    ),
    by = .socioecon_groups
  ]
  within_group[
    ,
    theil_share := group_theil_t * total_access / sum(total_access),
    by = .groups
  ]
  within_group[, component := "within-group"]
  within_group[, c("group_theil_t", "total_access") := NULL]

  # between group

  between_group <- data[
    ,
    .(
      total_access = as.numeric(sum(get(..opportunity) * get(..population))),
      population = sum(get(..population))
    ),
    by = .socioecon_groups
  ]
  between_group[
    ,
    `:=`(
      access_share = total_access / sum(total_access),
      pop_share = population / sum(population)
    ),
    by = .groups
  ]
  between_group[, theil_share := access_share * log(access_share / pop_share)]
  between_group[
    ,
    c("total_access", "population", "access_share", "pop_share") := NULL
  ]
  between_group[, component := "between-group"]

  decomposed_theil <- rbind(between_group, within_group)
  data.table::setcolorder(
    decomposed_theil,
    c(group_by, "component", socioeconomic_groups)
  )
  data.table::setorderv(
    decomposed_theil,
    c(group_by, "component", socioeconomic_groups)
  )

  output_list <- list(
    theil_t = theil_index,
    decomposed_theil_t = decomposed_theil
  )

  if (exists("original_class")) {
    class(output_list$theil_t) <- original_class
    class(output_list$decomposed_theil_t) <- original_class
  }

  return(output_list)
}

theil_t <- function(x, avg_x, weight) {
  stats::weighted.mean(x / avg_x * log(x / avg_x), w = weight)
}
