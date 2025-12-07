# Constrained accessibility

Calculates accessibility using constrained gravity models:

- `"total"`: Allocates total opportunities proportionally based on
  travel impedance.

- `"singly"`: Allocates opportunities proportionally, constrained on one
  side (demand or supply).

- `"doubly"`: Allocates flows so origin totals equal demand and
  destination totals equal supply.

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
  demand,
  supply,
  return_demand_side = NULL,
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

  A string. One of `"total"`, `"singly"`, or `"doubly"`.

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

- return_demand_side:

  Logical for `"total"` and `"singly"`, must be `NULL` for `"doubly"`.

- error_threshold:

  Numeric. Convergence criterion for doubly-constrained case.

- improvement_threshold:

  Numeric. Convergence criterion for improvement.

- max_iterations:

  Integer. Maximum iterations for doubly-constrained calibration.

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

See individual function documentation for mathematical details:
[`total_constrained()`](https://ipeagit.github.io/accessibility/dev/reference/total_constrained.md),
[`singly_constrained()`](https://ipeagit.github.io/accessibility/dev/reference/singly_constrained.md),
[`doubly_constrained()`](https://ipeagit.github.io/accessibility/dev/reference/doubly_constrained.md).

## Examples

``` r
# Load demo data shipped with the package (used for 'total' and 'singly')
data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

# Total-constrained (supply-side)
constrained_accessibility("total", travel_matrix, land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = NULL,
  supply          = "jobs",
  return_demand_side = FALSE
)
#>              from_id   supply
#>               <char>    <num>
#>   1: 89a88cdb57bffff 263.8175
#>   2: 89a88cdb597ffff 264.7306
#>   3: 89a88cdb5b3ffff 319.5332
#>   4: 89a88cdb5cfffff 428.2523
#>   5: 89a88cd909bffff 291.7166
#>  ---                         
#> 894: 89a881acda3ffff 280.1091
#> 895: 89a88cdb543ffff 697.2299
#> 896: 89a88cda667ffff 346.8143
#> 897: 89a88cd900fffff 113.1569
#> 898: 89a881aebafffff   0.0000

# Singly-constrained (demand-side)
constrained_accessibility("singly", travel_matrix, land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = "jobs",
  return_demand_side = TRUE
)
#>                to_id    demand
#>               <char>     <num>
#>   1: 89a88cdb57bffff  67.95218
#>   2: 89a88cdb597ffff 661.02294
#>   3: 89a88cdb5b3ffff 228.67413
#>   4: 89a88cdb5cfffff  88.35283
#>   5: 89a88cd909bffff   0.00000
#>  ---                          
#> 894: 89a881ae92fffff   0.00000
#> 895: 89a881ae923ffff   0.00000
#> 896: 89a881ae9afffff   0.00000
#> 897: 89a88cd8407ffff  36.87961
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

constrained_accessibility("doubly", tm_small, lu_small,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = "jobs",
  return_demand_side = NULL
)
#> Warning: Aggregated results equal marginals (O_i or D_j). Interpret with caution.
#>        id  flow
#>    <fctr> <num>
#> 1:      1     4
#> 2:      2    10
#> 3:      3     6
#> 4:      1     7
#> 5:      2     5
#> 6:      3     8
```
