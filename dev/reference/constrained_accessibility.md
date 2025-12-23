# Constrained accessibility

Calculates accessibility using constraints, as proposed in Soukhov et
al. (2025) . Accessibility is conceptualised as potential' spatial
interaction. The results are in units of opportunities in the region
(i.e., active accessibility `active = TRUE`) or in units of population
in the region (if `active = FALSE` reflecting passive accessibility).
Results are presented as a sum of a zone or presented as an
origin-destination flow (if `detailed_results = TRUE`). Please see
Details section for more information.

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
  active = NULL,
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
  origin). When `FALSE`, it calculates passive accessibility (by how
  many people each destination can be reached), which is equivalent to
  the notion of market potential. This parameter only works for
  `constraint` types `"total"` and `"singly"`. Ignored for
  `constraint = "doubly"`.

- error_threshold:

  Numeric. Convergence criterion used only for calibration in the
  doubly-constrained case (`constraint = "doubly"`).

- improvement_threshold:

  Numeric. Convergence criterion for improvement used only for
  calibration in the doubly-constrained case (`constraint = "doubly"`).

- max_iterations:

  Integer. Maximum iterations used only for calibration in the
  doubly-constrained case (`constraint = "doubly"`).

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

The following three constraint cases (along with their `active` and
passive variants) are included:

- `"total"`: Allocates the system-wide total proportionally based on
  travel impedance.

- If `active = TRUE`, results are in units of **opportunities** (supply)
  accessible by the origin. Only `supply` can be passed, `demand` must
  be NULL.

- If `active = FALSE`, results are in units of **population** (demand)
  accessible by the destination. Only `demand` can be passed, `supply`
  must be NULL.

- If `detailed_results = TRUE`, accessibility (in units corresponding to
  the logical for `active`) is presented as a flow along with
  intermediates.

- `"singly"`: Applies a single constraint, allocating one side of the
  marginal proportionally based on the other marginal and travel
  impedance:

- If `active = TRUE`, returns origin-side results (how much supply is
  accessible from each origin). Outputs are units of supply. `demand`
  and `supply` must be passed.

- If `active = FALSE`, returns destination-side results (how much demand
  is accessible from each destination). Outputs are units of demand.
  `demand` and `supply` must be passed.

- If `detailed_results = TRUE`, accessibility (in units corresponding to
  the logical for `active`) is presented as a flow along with
  intermediates.

- `"doubly"`: Allocates flows so supply at each destination matches
  demand at each origin.

- OD flows are calibrated to both marginals. OD flows are calibrated to
  both marginals using iterative proportional fitting. The sum of
  `demand` and `supply` must match; otherwise, the function will not
  converge.

- `active` and `detailed_results` must be NULL. Since supply must match
  demand, their units are the same and there is no distinction between
  'active' and 'passive' notions.

Please see the Details section for more information.

## Details

This function covers the family of constrained accessibility measures
proposed in Soukhov et al. (2025) .

### Total Constrained Accessibility

Allocates the total system-wide quantity proportionally based on travel
impedance between origins and destinations. This measure uses the logic
of a total ~(or 'unconstrained' by Wilson's terms)~ constraint.

Use this measure when the total quantity of **supply** OR **demand** in
the system is known and representing accessibility as a proportion of
this total is meaningful.

**Requirements**:

- Either `demand` or `supply` must be provided (cannot provide both).

**Interpretation**:

- `active = TRUE` (*active accessibility*): Results represent the total
  number of **opportunities** (supply) accessible from each origin based
  on region-relative travel impedance. The units are in 'supply' (e.g.,
  jobs, school seats).

  - If `detailed_results = FALSE`, outputs are aggregated and returned
    by origin.

  - If `detailed_results = TRUE`, OD-level flows are returned. Summing
    flows by origin equals the aggregated result.

- `active = FALSE` (*passive accessibility*, the notion of market
  potential): Results represent the total number of **population**
  (demand) that can reach each destination based on region-relative
  travel impedance. The units are in 'demand' (e.g., population).

  - If `detailed_results = FALSE`, outputs are aggregated by
    destination.

  - If `detailed_results = TRUE`, OD-level flows are returned. Summing
    flows by destination equals the aggregated result.

**Use cases**:

- Active accessibility (aggregated): "How many jobs can be reached from
  origin zone A given its region-relative travel impedance?"

- Active accessibility (flow-level): "How many jobs can be reached by
  flow A-\>1 given A-\>1's region-relative travel impedance?"

- Passive accessibility (aggregated): "How many people can reach
  destination zone 1 given its region-relative travel impedance?"

- Passive accessibility (flow-level): "How many people are reached by
  flow 1-\>A given 1-\>A's region-relative travel impedance?"

### Singly Constrained Accessibility

Allocates opportunities at each destination (or population at each
origin) proportionally based on travel impedance and the opposite
marginal. This measure uses the logic of single constraint from Wilson
(1971) .

Use this measure when modeling **competition**, where both demand and
supply are conceptualised to influence accessibility but only one side
is fixed. The measure distributes flows so that totals match the
constrained side while weighting by travel impedance and the
unconstrained side.

**Requirements**:

- Both `demand` and `supply` must be provided (the logical for `active`
  determines if either demand or supply is constrained).

**Interpretation**:

- `active = TRUE` (*active accessibility*): constrains supply. Results
  represent the total number of **opportunities** (supply) accessible
  from each origin based on region-relative travel impedance and
  population at the origin. The units are in 'supply' (e.g., jobs,
  school seats).

  - If `detailed_results = FALSE`, outputs are aggregated and returned
    by origin.

  - If `detailed_results = TRUE`, OD-level flows are returned. Summing
    flows by origin equals the aggregated result.

- `active = FALSE` (*passive accessibility*, the notion of market
  potential): constrains demand. Results represent the total number of
  **population** (demand) that can reach each destination based on
  region-relative travel impedance and opportunities at the destination.
  The units are in 'demand' (e.g., population).

  - If `detailed_results = FALSE`, outputs are aggregated by
    destination.

  - If `detailed_results = TRUE`, OD-level flows are returned. Summing
    flows by destination equals the aggregated result.

**Use cases**:

- Active accessibility (aggregated): "How many jobs can be reached from
  origin zone A given its region-relative travel impedance and demand?"

- Active accessibility (flow-level): "How many jobs can be reached by
  flow A-\>1 given A-\>1's region-relative travel impedance and demand?"

- Passive accessibility (aggregated): "How many people can reach
  destination zone 1 given its region-relative travel impedance and
  supply?"

- Passive accessibility (flow-level): "How many people are reached by
  flow 1-\>A given 1-\>A's region-relative travel impedance and supply?"

**NOTE:** the active form of this measure yields equivalent results to
the
[`spatial_availability()`](https://ipeagit.github.io/accessibility/dev/reference/spatial_availability.md)
function, through different logic.

### Doubly Constrained Accessibility

Allocates flows between origins and destinations using Wilson's
*doubly-constrained* gravity model Wilson (1971) .

The model uses iterative proportional fitting to update balancing
factors (`A_i` for origins and `B_j` for destinations) until
convergence. This guarantees that flows satisfy both marginals while
being weighted by travel impedance.

**Requirements**:

- Both `demand` and `supply` must be provided.

- Unlike `total` and `singly`, `doubly` requires the sum of demand and
  supply to match; otherwise, the model will not converge.

**Interpretation**:

- When `detailed_results = TRUE`, results include OD-level flows
  (`flow`) along with balancing factors (`A_i`, `B_j`) and travel
  impedance weights. The resulting flows represent the distribution of
  demand and supply across all origin-destination pairs. NOTE: OD flows
  are in flow units (jointly determined by demand and supply).

- When `detailed_results = FALSE`, results are not returned. As the
  aggregated outputs simply return the supply at destinations or demand
  at origin that was fed into `demand` and `supply` parameters.
  `detailed_results = TRUE` should only be used.

**Use cases**:

- flow-level: "What is the count of A-\>1 flows given A-\>1's
  region-relative travel impedance, demand and supply?"

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

# Total-constrained (active accessibility, aggregated): returns units of
# accessible supply by origin (requires supply)
constrained_accessibility(
  constraint =   "total",
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = NULL,
  supply          = "jobs",
  active = TRUE,
  detailed_results = FALSE
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

# Total-constrained (passive accessibility, aggregated): returns units of
# accessible demand by destination  (requires demand)
constrained_accessibility(
  constraint =   "total",
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = NULL,
  active = FALSE,
  detailed_results = FALSE
)
#>                to_id       demand
#>               <char>        <num>
#>   1: 89a88cdb57bffff  720.1584302
#>   2: 89a88cdb597ffff  958.7228358
#>   3: 89a88cdb5b3ffff 1074.8576863
#>   4: 89a88cdb5cfffff  632.9250575
#>   5: 89a88cd909bffff  882.5883640
#>  ---                             
#> 894: 89a881ae92fffff   13.4118931
#> 895: 89a881ae923ffff    0.2403741
#> 896: 89a881ae9afffff    3.6023628
#> 897: 89a88cd8407ffff   27.6942661
#> 898: 89a881aebafffff    0.0000000

# Singly-constrained (active accessibility, aggregated): returns units of
# accessible supply by origin (requires supply and demand)
constrained_accessibility(
  constraint =   "singly",
  travel_matrix = travel_matrix,
  land_use_data = land_use_data,
  travel_cost     = "travel_time",
  decay_function  = decay_exponential(0.1),
  demand          = "population",
  supply          = "jobs",
  active = TRUE,
  detailed_results = FALSE
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
#> 898: 89a881aebafffff    0.0000

# Doubly-constrained: returns units of flow (requires both demand and supply
# (totals that match) and `detailed_results = TRUE`)

# Using a small toy dataset with matching totals.
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
  supply          = "jobs",
  detailed_results = TRUE
)
#>    from_id  to_id       flow       A_i      B_j kappa_doubly        error
#>     <fctr> <fctr>      <num>     <num>    <num>        <num>        <num>
#> 1:       1      1 2.52693730 0.1721781 1.424797   0.36099104 0.0007282441
#> 2:       1      2 0.09995405 0.1721781 0.583010   0.01999081 0.0007282441
#> 3:       1      3 1.37651660 0.1721781 1.119684   0.17206457 0.0007282441
#> 4:       2      1 2.15341665 0.4336710 1.424797   0.30763095 0.0007282441
#> 5:       2      2 4.65063122 0.4336710 0.583010   0.93012624 0.0007282441
#> 6:       2      3 3.18866969 0.4336710 1.119684   0.39858371 0.0007282441
#> 7:       3      1 2.31964604 0.1737245 1.424797   0.33137801 0.0007282441
#> 8:       3      2 0.24941474 0.1737245 0.583010   0.04988295 0.0007282441
#> 9:       3      3 3.43481370 0.1737245 1.119684   0.42935171 0.0007282441
```
