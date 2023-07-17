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
#' ti <- theil_index2(
#'   accessibility_data = access,
#'   sociodemographic_data = land_use_data,
#'   opportunity = "jobs",
#'   population = "population",
#'   socioeconomic_groups = "income_decile"
#' )
#' ti
#'
#' @export
theil_index2 <- function(accessibility_data,
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
    c(population, socioeconomic_groups)
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
  .socioeconomic_groups <- socioeconomic_groups
  .socioecon_groups <- c(group_by, socioeconomic_groups)

  # we have to filter by opportunity because opportunity = 0 results in
  # theil_t being NaN (because log(0) is NaN)

  # we should probably print a WARNING if there are any 0 or missing values !!!!!!!!!!!!!!!!!!!!!!!!!
  data <- data[get(.opp_colname) > 0 ]
  data <- data[!is.na(get(.opp_colname))]
  data <- data[!is.na(get(.socioeconomic_groups))]
  if (length(group_by) > 0) { data <- data[!is.na(get(.groups))] }


  # calculate total average accessibility
  data[
    ,
    avg_access := stats::weighted.mean(
      get(..opportunity),
      w = get(..population)
    ),
    by = .groups
  ]

  # calculate total accessibility
  data[
    ,
    total_access := sum( get(..opportunity) * get(..population) ),
    by = .groups
  ]

  # average accessibility of each group
  data[
    ,
    group_avg_access := stats::weighted.mean(x = get(..opportunity),
                                             w = get(..population)),
    by = .socioecon_groups
  ]


  # summary of each group
  group_summary <- data[
    ,
    .(
      # overall average access
      avg_access = avg_access[1L],

      # average accessibility of each group
      group_avg_access = group_avg_access[1L],

      # total accessibility of each group as % of total access
      group_total_access = sum( get(..opportunity) * get(..population) ) / total_access,

      # theil of each group
      group_theil_t = theil_t(x = get(..opportunity),
                              group_avg_access,
                              w = get(..population))
    ),
    by = .socioecon_groups]

  group_summary <- unique(group_summary)

  # total within inequality
  total_within <- group_summary[, .(within_ineq = sum(group_total_access * group_theil_t)),
                                by = .groups]

  # total between inequality
  total_between <- group_summary[, .(between_ineq = sum(group_total_access * log(group_avg_access/avg_access))),
                                 by = .groups]

  # total Theil T ineq
  theil_index <- data[,
    .(theil_t = theil_t(get(..opportunity), avg_access, get(..population))),
    by = .groups
  ]

  #### TOTAL ineq

  total_ineq <- cbind(total_within , total_between)
  total_ineq <- cbind(total_ineq, theil_index)

  suppressWarnings(
    total_ineq <- data.table::melt(total_ineq,
                                   id.vars = group_by,
                                   variable.name = 'component')
    )

  # contribution of each component to total inequality
  total_ineq[, contribution := value / value[which(component=='theil_t')],
             by = .groups ]



  #### WITHIN ineq

  # contribution of each component to total *WITHIN* inequality
  if (length(group_by) > 0) { within_component <- merge(group_summary , total_within) }
  if (length(group_by)== 0) { within_component <- group_summary
                              within_component$within_ineq <- total_within[[1]]
  }

  within_component <- within_component[,
                        .(
                        within_group = group_total_access * group_theil_t,
                        contribution_to_within = (group_total_access * group_theil_t) / within_ineq
                        ),
                        by = .socioecon_groups
                        ]


  #### BETWEEN ineq

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
  between_group[, between_share := access_share * log(access_share / pop_share), by = .socioecon_groups ]
  # between_group[, total_between := sum(between_share), by = .groups ]
  # between_component <- between_group[, .(contribution_to_between = between_share / total_between), by = .socioecon_groups ]
  between_group <- between_group[, .(between_share), by = .socioecon_groups]

 # it does not make sense to calculate the % contribution of each group to total between ineq 666

# reorder columns
  within_component <- within_component[order(get(.socioeconomic_groups))]
  between_group <- between_group[order(get(.socioeconomic_groups))]
  if (length(group_by) > 0) {
    between_group <- between_group[order(get(.groups))]
    within_component <- within_component[order(get(.groups))]
    total_ineq <- total_ineq[order(get(.groups))]
    }

  output_list <- list(
    theil_t = total_ineq,
    # decomposed_theil_t = within_component
    decomposed_within = within_component,
    decomposed_between = between_group
    )

  return(output_list)
}

theil_t <- function(x, avg_x, weight) {
  stats::weighted.mean(x / avg_x * log(x / avg_x), w = weight)
}
