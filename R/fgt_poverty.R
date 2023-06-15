#' Foster–Greer–Thorbecke (FGT) poverty measures
#'
#' Calculates the Foster–Greer–Thorbecke (FGT) indices, a family of poverty
#' measures that capture the extent and severity of poverty. The FGT family
#' comprises three indicators that differ based on the the `alpha` parameter,
#' which can receive values of 0, 1 or 2. The interpretation of FGT is different
#' for each value of `alpha`. With `alpha = 0`, FGT0 captures the *extent* of
#' poverty as a simple headcount, i.e. the proportion of peole below the poverty
#' line. With `alpha = 1`, FGT1 captures the *severity* of poverty as the
#' average percent distance between the poverty line and the accessibility of
#' individuals below the poverty line. FGT1 is also known as the “poverty gap
#' index”. Finally, with `alpha = 2`, FGT2 index simultaneously captures the
#' *extent and severity* of poverty by calculating the number of people below
#' the poverty line weighted by the size of the accessibility shortfall
#' relative to the poverty line. The value of FGT2 ranges from 0 to 1, where 0
#' indicates that every individual is above the poverty line, and a value of 1
#' indicates that the entire population is below the poverty line. The function
#' returns a `data.frame` with the values of FGT0, FGT1 and FGT2. The FGT
#' family of poverty measures was originally proposed by
#' \insertCite{foster1984class;textual}{accessibility}.
#'
#' @param accessibility_data A data frame with accessibility estimates. It must
#'   contain the columns `id` and any others specified in `opportunity`.
#' @param opportunity A string. The name of the column in `accessibility_data`
#'   with the accessibility estimates to be considerend when calculating
#'   accessibility poverty.
#' @param sociodemographic_data A data frame. The distribution of the population
#'   in the study area cells. Must contain the columns `id` and any others
#'   specified in `population`.
#' @param population A string. The name of the column in `sociodemographic_data`
#'   with the number of people in each cell. Used to weigh accessibility levels
#'   when calculating poverty.
#' @param poverty_line A `numeric` value. The value of the accessibility poverty
#'   line, below which individuals are considered to be in accessibility poverty.
#' @param group_by A `character` vector. When not `character(0)` (the default),
#'   indicates the `accessibility_data` columns that should be used to group the
#'   poverty estimates by. For example, if `accessibility_data` includes a
#'   `race` column that specifies the racial category of the population (e.g.
#'   `"Black"` and `"White"`) that each entry refers to, passing `"race"` to
#'   this parameter results in poverty estimates grouped by race.
#'
#' @return A data frame containing the poverty three estimates for the study
#'    area: FGT0, FGT1 and FGT2.
#'
#' @details
#'  # Interpretation of Alpha values:
#'  - With `alpha = 0`, the FGT0 index captures the *extent* of poverty as a
#'  simple headcount, i.e. the proportion of peole below the poverty line.
#'  - With `alpha = 1`, FGT1 captures the *severity* of poverty as the average
#'  percent distance between the poverty line and the accessibility of
#'  individuals below the poverty line. It is also known as the “poverty gap
#'  index”.
#'  - Finally, with `alpha = 2`, the FGT2 index simultaneously captures the
#'  *extent and severity* of poverty by calculating the number of people below
#'  the poverty line weighted by the size of the accessibility shortfall
#'  relative to the poverty line. The value of FGT2 ranges from 0 to 1, where 0
#'  indicates that every individual is above the poverty line, and a value of 1
#'  indicates that the entire population is below the poverty line.
#'
#' @family Poverty
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
#' poverty <- fgt_poverty(
#'   accessibility_data = access,
#'   opportunity = "jobs",
#'   sociodemographic_data = land_use_data,
#'   population = "population",
#'   poverty_line = 95368
#' )
#' poverty
#'
#' @export
fgt_poverty <- function(accessibility_data,
                        opportunity,
                        sociodemographic_data = NULL,
                        population = NULL,
                        poverty_line,
                        group_by = character(0)) {
  checkmate::assert_string(opportunity)
  checkmate::assert_string(population)
  checkmate::assert_numeric(poverty_line, lower = 0)
  assert_access_group_by(group_by)
  assert_accessibility_data(accessibility_data, opportunity, group_by)


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

  warn_extra_access_cols(accessibility_data, opportunity, group_by)


  .opportunity_colname <- opportunity
  .population_colname <- population


  data[, p := fifelse(get(.opportunity_colname) < poverty_line,
                      ((poverty_line - get(.opportunity_colname)) / poverty_line), 0)]

  data[, FGT0 := fifelse(get(.opportunity_colname) < poverty_line, p^0, 0) ]
  data[, FGT1 := fifelse(get(.opportunity_colname) < poverty_line, p^1, 0) ]
  data[, FGT2 := fifelse(get(.opportunity_colname) < poverty_line, p^2, 0) ]

  fgt <-
    data[, .(
      FGT0 = weighted.mean(x = FGT0, w = get(.population_colname)),
      FGT1 = weighted.mean(x = FGT1, w = get(.population_colname)),
      FGT2 = weighted.mean(x = FGT1, w = get(.population_colname))
    ),
    by = group_by]

  return(fgt[])
}
