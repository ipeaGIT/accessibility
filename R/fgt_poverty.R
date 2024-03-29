#' Foster-Greer-Thorbecke (FGT) poverty measures
#'
#' Calculates the FGT metrics, a family of poverty measures originally proposed
#' by \insertCite{foster1984class;textual}{accessibility} that capture the
#' extent and severity of poverty within an accessibility distribution. The FGT
#' family is composed of three measures that differ based on the \eqn{\alpha}
#' parameter used to calculate them (either 0, 1 or 2) and which also changes
#' their interpretation. Please see the details section for more information on
#' the interpretation of the measures.
#'
#' @param accessibility_data A data frame. The accessibility levels whose
#'   poverty levels should be calculated. Must contain the columns `id` and any
#'   others specified in `opportunity`.
#' @template sociodem_data_without_income
#' @param opportunity A string. The name of the column in `accessibility_data`
#'   with the accessibility levels to be considerend when calculating
#'   accessibility poverty.
#' @param population A string. The name of the column in `sociodemographic_data`
#'   with the number of people in each cell. Used to weigh accessibility levels
#'   when calculating poverty.
#' @param poverty_line A `numeric`. The poverty line below which individuals are
#'   considered to be in accessibility poverty.
#' @param group_by A `character` vector. When not `character(0)` (the default),
#'   indicates the `accessibility_data` columns that should be used to group the
#'   poverty estimates by. For example, if `accessibility_data` includes a
#'   `race` column that specifies the racial category of the population (e.g.
#'   `"black"` and `"white"`) that each entry refers to, passing `"race"` to
#'   this parameter results in poverty estimates grouped by race.
#'
#' @return A data frame containing the three poverty estimates (FGT0, FGT1 and
#'   FGT2) for the study area.
#'
#' @section Interpretation of FGT measures:
#' The interpretation of each FGT measure depends on the \eqn{\alpha} parameter
#' used to calculate it:
#'
#' - with \eqn{\alpha = 0} (FGT0) the measure captures the *extent* of poverty
#' as a simple headcount - i.e. the proportion of people below the poverty line;
#'
#' - with \eqn{\alpha = 1} (FGT1) the measure, also know as the "poverty gap
#' index", captures the *severity* of poverty as the average percentage distance
#' between the poverty line and the accessibility of individuals below the
#' poverty line;
#'
#' - with \eqn{\alpha = 2} (FGT2) the measure simultaneously captures the
#' *extent* and the *severity* of poverty by calculating the number of people
#' below the poverty line weighted by the size of the accessibility shortfall
#' relative to the poverty line.
#'
#' FGT values range from 0 to 1. A value of 0 indicates that every individual is
#' above the poverty line. When every individual is below the poverty line,
#' however, FGT0 value is 1 and FGT1 and FGT2 values approach 1.
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
#' poverty <- fgt_poverty(
#'   access,
#'   opportunity = "jobs",
#'   sociodemographic_data = land_use_data,
#'   population = "population",
#'   poverty_line = 95368
#' )
#' poverty
#'
#' @export
fgt_poverty <- function(accessibility_data,
                        sociodemographic_data,
                        opportunity,
                        population,
                        poverty_line,
                        group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  checkmate::assert_numeric(poverty_line, lower = 0)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)
  assert_sociodemographic_data(
    sociodemographic_data,
    accessibility_data,
    population = population
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

  warn_extra_access_cols(accessibility_data, opportunity, group_by)

  .opp_colname <- opportunity
  .pop_colname <- population
  .groups <- group_by

  data[get(.opp_colname) >= poverty_line, c(".fgt0", ".fgt1", ".fgt2") := 0]

  data[
    get(.opp_colname) < poverty_line,
    .norm_opp_shortfall := (poverty_line - get(.opp_colname)) / poverty_line
  ]
  data[
    get(.opp_colname) < poverty_line,
    `:=`(
      .fgt0 = .norm_opp_shortfall ^ 0,
      .fgt1 = .norm_opp_shortfall ^ 1,
      .fgt2 = .norm_opp_shortfall ^ 2
    )
  ]

  if (nrow(data) == 0 && identical(group_by, character(0))) {
    fgt <- data.table::data.table(
      FGT0 = numeric(0),
      FGT1 = numeric(0),
      FGT2 = numeric(0)
    )
  } else {
    fgt <- data[
      ,
      .(
        FGT0 = stats::weighted.mean(x = .fgt0, w = get(.pop_colname)),
        FGT1 = stats::weighted.mean(x = .fgt1, w = get(.pop_colname)),
        FGT2 = stats::weighted.mean(x = .fgt2, w = get(.pop_colname))
      ),
      by = .groups
    ]
  }

  if (exists("original_class")) class(fgt) <- original_class

  return(fgt[])
}
