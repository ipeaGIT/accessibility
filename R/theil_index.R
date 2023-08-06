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
#' # some cells are classified as in the decile NA. that's because their income
#' # per capita is NaN, as they don't have any population. we filter these cells
#' # from our accessibility data, otherwise the output would include NA values
#'
#' na_decile_ids <- land_use_data[is.na(land_use_data$income_decile), ]$id
#' access <- access[! access$id %in% na_decile_ids, ]
#' sociodem_data <- land_use_data[! land_use_data$id %in% na_decile_ids, ]
#'
#' ti <- theil_index(
#'   access,
#'   sociodemographic_data = sociodem_data,
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
  # TODO: throw warning

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
      group_tot_access = as.numeric(sum(get(..opportunity) * get(..population)))
    ),
    by = .socioecon_groups
  ]
  within_group[
    ,
    value := group_theil_t * group_tot_access / sum(group_tot_access),
    by = .groups
  ]
  within_group[
    ,
    share_of_component := value / sum(value),
    by = .groups
  ]
  within_group[, c("group_theil_t", "group_tot_access") := NULL]
  data.table::setorderv(within_group, .socioecon_groups)

  # between group

  between_group <- data[
    ,
    .(
      group_tot_access = as.numeric(
        sum(get(..opportunity) * get(..population))
      ),
      population = sum(get(..population))
    ),
    by = .socioecon_groups
  ]
  between_group[
    ,
    `:=`(
      access_share = group_tot_access / sum(group_tot_access),
      pop_share = population / sum(population)
    ),
    by = .groups
  ]
  between_group[, value := access_share * log(access_share / pop_share)]
  between_group[
    ,
    share_of_component := value / sum(value),
    by = .groups
  ]
  between_group[
    ,
    c("group_tot_access", "population", "access_share", "pop_share") := NULL
  ]
  data.table::setorderv(between_group, .socioecon_groups)

  output_list <- list(
    theil_t = theil_index,
    within_group_component = within_group,
    between_group_component = between_group
  )

  if (exists("original_class")) {
    class(output_list$theil_t) <- original_class
    class(output_list$within_group_component) <- original_class
    class(output_list$between_group_component) <- original_class
  }

  return(output_list)
}

theil_t <- function(x, avg_x, weight) {
  stats::weighted.mean(x / avg_x * log(x / avg_x), w = weight)
}
