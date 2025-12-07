# Foster-Greer-Thorbecke (FGT) poverty measures

Calculates the FGT metrics, a family of poverty measures originally
proposed by Foster et al. (1984) that capture the extent and severity of
poverty within an accessibility distribution. The FGT family is composed
of three measures that differ based on the \\\alpha\\ parameter used to
calculate them (either 0, 1 or 2) and which also changes their
interpretation. Please see the details section for more information on
the interpretation of the measures.

## Usage

``` r
fgt_poverty(
  accessibility_data,
  sociodemographic_data,
  opportunity,
  population,
  poverty_line,
  group_by = character(0)
)
```

## Arguments

- accessibility_data:

  A data frame. The accessibility levels whose poverty levels should be
  calculated. Must contain the columns `id` and any others specified in
  `opportunity`.

- sociodemographic_data:

  A data frame. The distribution of sociodemographic characteristics of
  the population in the study area cells. Must contain the columns `id`
  and any others specified in `population`.

- opportunity:

  A string. The name of the column in `accessibility_data` with the
  accessibility levels to be considerend when calculating accessibility
  poverty.

- population:

  A string. The name of the column in `sociodemographic_data` with the
  number of people in each cell. Used to weigh accessibility levels when
  calculating poverty.

- poverty_line:

  A `numeric`. The poverty line below which individuals are considered
  to be in accessibility poverty.

- group_by:

  A `character` vector. When not `character(0)` (the default), indicates
  the `accessibility_data` columns that should be used to group the
  poverty estimates by. For example, if `accessibility_data` includes a
  `race` column that specifies the racial category of the population
  (e.g. `"black"` and `"white"`) that each entry refers to, passing
  `"race"` to this parameter results in poverty estimates grouped by
  race.

## Value

A data frame containing the three poverty estimates (FGT0, FGT1 and
FGT2) for the study area.

## Interpretation of FGT measures

The interpretation of each FGT measure depends on the \\\alpha\\
parameter used to calculate it:

- with \\\alpha = 0\\ (FGT0) the measure captures the *extent* of
  poverty as a simple headcount - i.e. the proportion of people below
  the poverty line;

- with \\\alpha = 1\\ (FGT1) the measure, also know as the "poverty gap
  index", captures the *severity* of poverty as the average percentage
  distance between the poverty line and the accessibility of individuals
  below the poverty line;

- with \\\alpha = 2\\ (FGT2) the measure simultaneously captures the
  *extent* and the *severity* of poverty by calculating the number of
  people below the poverty line weighted by the size of the
  accessibility shortfall relative to the poverty line.

FGT values range from 0 to 1. A value of 0 indicates that every
individual is above the poverty line. When every individual is below the
poverty line, however, FGT0 value is 1 and FGT1 and FGT2 values approach
1.

## References

Foster J, Greer J, Thorbecke E (1984). “A Class of Decomposable Poverty
Measures.” *Econometrica*, **52**(3), 761–766. ISSN 0012-9682,
[doi:10.2307/1913475](https://doi.org/10.2307/1913475) , 1913475.

## Examples

``` r
data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

access <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  cutoff = 30,
  opportunity = "jobs",
  travel_cost = "travel_time"
)

poverty <- fgt_poverty(
  access,
  opportunity = "jobs",
  sociodemographic_data = land_use_data,
  population = "population",
  poverty_line = 95368
)
poverty
#>         FGT0      FGT1      FGT2
#>        <num>     <num>     <num>
#> 1: 0.5745378 0.3277383 0.2218769
```
