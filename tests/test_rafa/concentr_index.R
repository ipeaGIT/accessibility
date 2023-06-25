tentando a formula dos papers
#' Concentration index
#'
#' Calculates the Concentration Index to measure the socioeconomic inequality of
#' access to opportunities. It measures the extent to which inequalities in
#' access to opportunities are systematically associated with individuals'
#' socioeconomic levels.
#'
#'
#' @template accessibility_data
#' @template sociodem_data_without_income
#' @template opportunity_access
#' @template population
#' @template ses
#' @template type
#' @template group_by_access
#'
#' @template return_inequality
#'
#' @section The concentration index can be defined as a normalized sum of
#' weighted accessibility levels, with the weights being determined by the
#' socioeconomic rank of each individual. This means the CI can theoretically
#' vary between -1 and +1 (when all accessibility is concentrated in the most or
#' in the least disadvantaged person, respectively). Negative values indicate
#' that inequalities favor the poor, while positive values indicate a pro-rich
#' bias. As such, one advantage of CI is that the sign and the magnitude of the
#' index indicate, respectively, the direction and the strength of the
#' relationship between accessibility and socioeconomic position.
#'
#' @family inequality
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
                         type,
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

  # rank individuals
  data[, rank := 1:.N, by = .groups]

  ### fractional rank (for weighted data)
  # Eddy van Doorslaer and Xander Koolman (4)
  # https://doi.org/10.1002/hec.918
  data[, pop_share := get(.pop_colname)/sum(get(.pop_colname)) , by = .groups]
  data[, frank := (c(0, cumsum(pop_share)[-max(rank)]) + pop_share/2), by = .groups]

  data[, fw := frank - 0.5, by = .groups] # fractional weight

  # weight for access level of each individual
  data[, w := rank - ((.N + 1)/2), by = .groups]


  ### calcuate CI

    # standard CI
    if (type == 'CI') {

      ### unweighted ok
      #> Erreygers (2009) (24)
      #> https://doi.org/10.1016/j.jhealeco.2008.02.003
      data[, avg_access := mean(x = get(.opp_colname)), by = .groups]
      ci <- data[, .(CI = (2 / (.N^2 * (avg_access))) *
                      sum(get(.opp_colname) * w)
                    ), by = .groups]
      # ok 0.2411739

      ### weighted ok
      #> Eddy van Doorslaer and Xander Koolman (2004) (2)
      #> https://doi.org/10.1002/hec.918
      data[, avg_access := weighted.mean(x = get(.opp_colname),
                                         w = get(.pop_colname)
                                         ), by = .groups]

      ci <-  data[, .(CI = (2 / (sum(get(.pop_colname)) * (avg_access))) *
                        sum(get(.opp_colname) * get(.pop_colname) * frank) - 1
                      ), by = .groups]
      # ok 0.2864975

    }

    # # corrected CI
    if (type == 'CIc') {

      # upper and lower bounds
      data[, upper := max(get(.opp_colname), na.rm=TRUE), by = .groups] # ? by = .groups
      data[, lower := min(get(.opp_colname), na.rm=TRUE), by = .groups] # ? by = .groups
      # ou seriam max e min teoricos, e nao os observados

      ### unweighted ok
      #> Erreygers (2009) (27)
      #> https://doi.org/10.1016/j.jhealeco.2008.02.003

      # 0.262953
      ci <- data[, .(CI =
                       (8 / (.N ^ 2 * (upper - lower))) *
                       sum(w * get(.opp_colname))
                     ), by = .groups]


      ### weighted (unweighted + complemento do rineq)
      # 0.3346494
      ci <- data[, .(CI = ((2 / (sum(get(.pop_colname)) * (avg_access))) *
                 sum(get(.opp_colname) * get(.pop_colname) * frank) - 1) *
                   4 * ( avg_access / (upper -lower)) # complemento do ineq
                 ), by = .groups]
    }


  # return output
  ci <- unique(ci)
  return(ci)
}


library(data.table)
df <- merge(access,
            land_use_data[, .(id, population, income_per_capita)]
            , by = 'id') |> setDT()

# https://github.com/kdevkdev/rineq
rineq::ci( ineqvar = df$income_per_capita,
           outcome = df$jobs ,
           method='direct',
           weights = df$population,
           type= 'CI')
#> 0.241175
#> WWW 0.2865013


 concentration_index(
   accessibility_data = access,
   sociodemographic_data = land_use_data,
   opportunity = "jobs",
   population = "population",
   income = "income_per_capita",
   type = "CI")

# 0.2411739  unweighted OK


 #### --------------------------


 rineq::ci( ineqvar = df$income_per_capita,
            outcome = df$jobs ,
            method='direct',
            weights = df$population,
            type= 'CIc')
 #> 0.262953
 #> WWW 0.3346494



 concentration_index(
   accessibility_data = access,
   sociodemographic_data = land_use_data,
   opportunity = "jobs",
   population = "population",
   income = "income_per_capita",
   type = "CIc")

 0.2629539
