#' Concentration index
#'
#' Calculates the Concentration Index to measure the socioeconomic inequality of
#' access to opportunities. It measures the extent to which inequalities in
#' accessibility are systematically associated with individuals' socioeconomic
#' levels. The function currently include the options of calculating the
#' corrected concentration index proposed by \insertCite{erreygers2009correcting;textual}{accessibility}
#' and the standard relative concentration index.
#'
#' @template accessibility_data
#' @param sociodemographic_data A data frame. The distribution of
#'   sociodemographic characteristics of the population in the study area cells.
#'   Must contain the columns `id` and any others specified in `population` and
#'   `income`.
#' @template opportunity_access
#' @template population
#' @param income A string. The name of column in `sociodemographic_data` with
#'        the income variable that should be used to classify the population in
#'        socioeconomic groups. Please note that this variable should describe
#'        income per capita (e.g. mean income per capita, household income per
#'        capita, etc), instead of the total amount of income in each cell.
#'        ?? income or SES ? daniel
#' @param type A string. the type of concentration index to be calculated.
#'        Options include "CIc" for the Corrected concentration index proposed
#'        by Erreygers (2009) (the default), and "CI" for the the standard
#'        relative concentration index.
#' @param group_by A `character` vector. When not `character(0)` (the default),
#'   indicates the `accessibility_data` columns that should be used to group the
#'   poverty estimates by. For example, if `accessibility_data` includes a
#'   `"scenario"` column that specifies the accessibility estimates `"before"`
#'   and `"after"` given a transport policy intervention, passing `"scenario"` to
#'   this parameter results in inequality estimates for each scenario.
#'
#' @template return_inequality
#'
#' @family inequality
#'
#' @section Interpretation of the concentration index:
#' The concentration index can theoretically vary between -1 and +1
#' (when all accessibility is concentrated in the most or in the least
#' disadvantaged person, respectively). Negative values indicate
#' that inequalities favor the poor, while positive values indicate a pro-rich
#' bias.
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
#' ci <- concentration_index(
#'   accessibility_data = access,
#'   sociodemographic_data = land_use_data,
#'   opportunity = "jobs",
#'   population = "population",
#'   income = "income_per_capita",
#'   type = "CIc"
#' )
#' ci
#'
#' @export
concentration_index <- function(
                         accessibility_data,
                         sociodemographic_data,
                         opportunity,
                         population,
                         income,
                         type = "CIc",
                         group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  checkmate::assert_string(income)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)
  assert_sociodemographic_data(sociodemographic_data, c(population, income))


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

  .opp_colname <- opportunity
  .pop_colname <- population
  .inc_colname <- income
  .groups <- group_by


  # remove obs with no pop na missing income
  data <- data[ get(.pop_colname) >0 ]
  data <- data[ ! is.na(get(.inc_colname)) ]
  data <- data[ ! is.na(get(.opp_colname)) ]

  ## order observations by SES
  data.table::setorderv(data, cols = c(group_by, income, population))


  ### fractional rank (for weighted data) of the income variable
  # Eddy van Doorslaer and Xander Koolman (4)
  # https://doi.org/10.1002/hec.918
  data[, rank := 1:.N, by = .groups]
  data[, pop_share := get(.pop_colname)/sum(get(.pop_colname)) , by = .groups]
  data[, weighted_rank := (c(0, cumsum(pop_share)[-max(rank)]) + pop_share/2), by = .groups]
  # data[, weighted_rank := rineq::rank_wt(get(.inc_colname), wt = get(.pop_colname)), by = .groups]

  # calculate average rank
  data[, meanw_rank := mean(weighted_rank), by = .groups]

  # weight for access level of each individual
  # data[, w := rank - ((.N + 1)/2), by = .groups]
  data[, weights_norm := get(.pop_colname) / sum(get(.pop_colname)), by = .groups]


  # weighted average access
  data[, avg_access := stats::weighted.mean(x = get(.opp_colname),
                                     w = get(.pop_colname)
                                     ), by = .groups]

  # order df
  data.table::setorderv(data, cols = c(group_by, 'weighted_rank'))


  ### calcuate CI

  # standard CI
  if (type == 'CI') {
    ci <- data[, .(CI = 2 * sum(
            weights_norm * (get(.opp_colname) - avg_access) *
              (weighted_rank - meanw_rank) / avg_access)
            )
          , by = .groups]
    }


  # corrected CI, see Erreygers (2009)
  if (type == 'CIc') {
    # upper and lower bounds
    data[, upper := max(get(.opp_colname), na.rm = TRUE), by = .groups]
    data[, lower := min(get(.opp_colname), na.rm = TRUE), by = .groups]
    # ou seriam max e min teoricos, e nao os observados

    ci <- data[, .(CI = 8 * sum(
            weights_norm * (get(.opp_colname) - avg_access) *
              (weighted_rank - meanw_rank) / avg_access
            ) * avg_access / (upper - lower)
          )
          , by = .groups]
      }


  # return output
  ci <- unique(ci)
  return(ci)
}
