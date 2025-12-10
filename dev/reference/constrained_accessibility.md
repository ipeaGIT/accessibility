# Constrained accessibility

Calculates accessibility using constrained gravity models, as proposed
in Soukhov et al. (2025) :

- `"total"`: Allocates total opportunities proportionally based on
  travel impedance.

- `"singly"`: Allocates opportunities proportionally, constrained on one
  side (demand or supply).

- `"doubly"`: Allocates flows so origin totals equal demand and
  destination totals equal supply. Please see the Details section for
  more information.

This function is generic over any kind of numeric travel cost, such as
distance, time and money.

## Usage

``` r
constrained_accessibility(
  constraint,
  travel_matrix,
  land_use_data,
  travel_cost,
  decay_function,
  demand = NULL,
  supply = NULL,
  active = TRUE,
  error_threshold = 0.001,
  improvement_threshold = 1e-06,
  max_iterations = 1000,
  group_by = character(0),
  fill_missing_ids = TRUE,
  detailed_results = FALSE
)
```

## Arguments

- constraint:

  A string. One of `"total"`, `"singly"`, or `"doubly"`. See Details
  section for more information.

- travel_matrix:

  A data frame. The travel matrix describing the costs (i.e. travel
  time, distance, monetary cost, etc.) between the origins and
  destinations in the study area. Must contain the columns `from_id`,
  `to_id` and any others specified in `travel_cost`.

- land_use_data:

  A data frame. The distribution of opportunities within the study area
  cells. Must contain the columns `id` and any others specified in
  `opportunity`.

- travel_cost:

  A string. The name of the column in `travel_matrix` with the travel
  cost between origins and destinations.

- decay_function:

  A `fuction` that converts travel cost into an impedance factor used to
  weight opportunities. This function should take a `numeric` vector and
  also return a `numeric` vector as output, with the same length as the
  input. For convenience, the package currently includes the following
  functions:
  [`decay_binary()`](https://ipeagit.github.io/accessibility/dev/reference/decay_binary.md),
  [`decay_exponential()`](https://ipeagit.github.io/accessibility/dev/reference/decay_exponential.md),
  [`decay_power()`](https://ipeagit.github.io/accessibility/dev/reference/decay_power.md)
  and
  [`decay_stepped()`](https://ipeagit.github.io/accessibility/dev/reference/decay_stepped.md).
  See the documentation of each decay function for more details.

- demand:

  A string. The name of the column in `land_use_data` with the number of
  opportunity-demanders at each origin (e.g., people) that will be
  considered.

- supply:

  A string. The name of the column in `land_use_data` with the number of
  opportunity supply at each destination (e.g., jobs, school-seats) that
  will be considered.

- active:

  A logical. When `TRUE`, the function calculates active accessibility
  (the quantity of opportunities that can be reached from a given
  origin). when `FALSE`, it calculates passive accessibility (by how
  many people each destination can be reached), which is equivalent to
  the notion of market potential. This parameter only works for
  `constraint` types `"total"` and `"singly"`. Ignored for
  `constraint = "doubly"`.

- error_threshold:

  Numeric. Convergence criterion used only for doubly-constrained case.

- improvement_threshold:

  Numeric. Convergence criterion for improvement used only for
  doubly-constrained case.

- max_iterations:

  Integer. Maximum iterations used only for doubly-constrained
  calibration.

- group_by:

  A `character` vector. When not `character(0)` (the default), indicates
  the `travel_matrix` columns that should be used to group the
  accessibility estimates by. For example, if `travel_matrix` includes a
  departure time column, that specifies the departure time of each entry
  in the data frame, passing `"departure_time"` to this parameter
  results in accessibility estimates grouped by origin and by departure
  time.

- fill_missing_ids:

  A `logical`. When calculating grouped accessibility estimates (i.e.
  when `by_col` is not `NULL`), some combinations of groups and origins
  may be missing. For example, if a single trip can depart from origin
  `A` at 7:15am and reach destination `B` within 55 minutes, but no
  trips departing from `A` at 7:30am can be completed at all, this
  second combination will not be included in the output. When `TRUE`
  (the default), the function identifies which combinations would be
  left out and fills their respective accessibility values with 0, which
  incurs in a performance penalty.

- detailed_results:

  Logical. Whether to return detailed OD-level results.

## Details

This function covers the family of constrained accessibility measures
proposed in Soukhov et al. (2025) .

### Total constrained accessibility

Sum of accessibility equals total opportunities (supply) in the region.
It allocates total opportunities in the region proportionally based on
travel impedance. Uses the logic of a total ~(or unconstrained by
Wilson's terms)~ constraint. Returns values as either `demand` or
`supply`. When `active = FALSE` (market potential variant) is also
available.

### Singly constrained accessibility

Allocates opportunities at each destination proportionally based on
travel impedance and population at the origin. Uses the logic of single
constraint from Wilson (1971) . Returns values as either 'demand' or
'supply'. Supply-constrained (destination totals fixed) when
`market_potential = FALSE`. In either case, totals match either the
demand at each origin or supply at each destination, depending on
variant. This is equivalent to the
[`spatial_availability()`](https://ipeagit.github.io/accessibility/dev/reference/spatial_availability.md)
function.

### Doubly constrained accessibility

Calculates accessibility using doubly-constrained gravity model of
Wilson (1971) . This measure allocates flows between origins and
destinations such that origin totals equal demand and destination totals
equal supply. Iterative proportional fitting updates (A_i) and (B_j)
until convergence. This ensures that row sums equal (O_i) (demand) and
column sums equal (D_j) (supply). Note, only OD-level outputs are
available (as aggregate outputs just match inputs).

## References

Soukhov A, Pereira RH, Higgins CD, Páez A (2025). “A family of
accessibility measures derived from spatial interaction principles.”
*PLoS One*, **20**(11), e0335951.  
  
Wilson AG (1971). “A family of spatial interaction models, and
associated developments.” *Environment and Planning A*, **3**(1), 1–32.

## See also

Other Constrained accessibility:
[`spatial_availability()`](https://ipeagit.github.io/accessibility/dev/reference/spatial_availability.md)

## Examples

``` r
# Load demo data shipped with the package
data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

# Total-constrained (supply-side)
constrained_accessibility(
  constraint =   "total",
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = NULL,
  supply          = "jobs",
  active = FALSE
)
#> Error in total_constrained(travel_matrix = travel_matrix, land_use_data = land_use_data,     travel_cost = travel_cost, decay_function = decay_function,     group_by = group_by, fill_missing_ids = fill_missing_ids,     detailed_results = detailed_results, active = active, demand = if (!active) demand else NULL,     supply = if (active) supply else NULL): For active = FALSE, demand must be specified and supply must be NULL.

# Singly-constrained (demand-side)
constrained_accessibility(
  constraint =   "singly",
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = "jobs",
  active = TRUE
)
#>              from_id    supply
#>               <char>     <num>
#>   1: 89a88cdb57bffff  186.0876
#>   2: 89a88cdb597ffff  140.0738
#>   3: 89a88cdb5b3ffff  736.5830
#>   4: 89a88cdb5cfffff  900.9284
#>   5: 89a88cd909bffff    0.0000
#>  ---                          
#> 894: 89a881acda3ffff  453.8818
#> 895: 89a88cdb543ffff 1184.0239
#> 896: 89a88cda667ffff  276.2530
#> 897: 89a88cd900fffff  103.5370
#> 898: 89a881aebafffff       NaN

# Doubly-constrained: use a small toy dataset with matching totals
tm_small <- data.table::data.table(
  expand.grid(from_id = c("1","2","3"), to_id = c("1","2","3"))
)
tm_small[, travel_time := c(10, 30, 15, 30, 10, 25, 15, 25, 10)]
#>    from_id  to_id travel_time
#>     <fctr> <fctr>       <num>
#> 1:       1      1          10
#> 2:       2      1          30
#> 3:       3      1          15
#> 4:       1      2          30
#> 5:       2      2          10
#> 6:       3      2          25
#> 7:       1      3          15
#> 8:       2      3          25
#> 9:       3      3          10
lu_small <- data.table::data.table(
  id         = c("1","2","3"),
  population = c(4, 10, 6),   # sum = 20
  jobs       = c(7,  5,  8)   # sum = 20
)

constrained_accessibility(
  constraint = "doubly",
  travel_matrix = tm_small,
  land_use_data = lu_small,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = "jobs"
)
#> Error in constrained_accessibility(constraint = "doubly", travel_matrix = tm_small,     land_use_data = lu_small, travel_cost = "travel_time", decay_function = decay_exponential(0.1),     demand = "population", supply = "jobs"): For 'doubly', the 'active' parameter is not used.
```
