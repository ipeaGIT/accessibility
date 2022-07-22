# accessibility 1.0.0

The package has been to tremendous changes. Basically, there's not a single
part of it that remained untouched: documentation, vignettes, function names,
parameter names, extra functionality, performance improvements, etc. While it
is impossible to highlight everything that has been done, we'll try to summary
some of the key points in the following topics.

## Breaking changes

- Accessibility functions previously worked with a single input dataset:
  `data`. Now they require two input datasets: `travel_matrix` and
  `land_use_data`.
- Function names were changed:
  - `time_to_closest()` -> `cost_to_closest()`
  - `cumulative_time_cutoff()` -> `cumulative_cutoff()`
  - `cumulative_time_interval()` -> `cumulative_interval()`
  - `gravity_access()` -> `gravity()`
- Parameter names were changed:
  - `opportunity_col` -> `opportunity`
  - `travel_cost_col` -> `travel_cost`
  - `by_col` -> `active`
  - In `cost_to_closest()`: `n_opportunities` -> `n`
  - In `cumulative_interval()`: `stat` -> `summary_function`
  - In `floating_catchment_area()`: `population_col` -> `demand`
  - In `floating_catchment_area()`: `fca_metric` -> `method`
- Parameter required values were changed:
  - `active` now takes a `logical`, instead of a string (which `by_col`
    previously took).
  - In `cumulative_interval()`: `summary_function` now takes a `function`,
    instead of a string (which `stat` previously took).

## New features

- New function `decay_stepped()`.
- New parameter `interval_increment` to `cumulative_interval()`, used to
  specify how many travel cost units separate the cutoffs used to calculate the
  accessibility estimates which will be used to calculate the summary estimate
  within the specified interval.
- All accessibility functions gained a `group_by` parameter, that allows
  accessibility estimates to be grouped by one or more columns present in
  `travel_matrix`.
- All accessibility functions (but `cumulative_interval()`) gained a
  `fill_missing_ids` parameter, that includes in the results origins whose
  accessibility would be 0 but, due to some commonly overlooked implementation
  details, are usually left out from the output. `cumulative_interval()` doesn't
  have this parameter because its result will always include all origins,
  otherwise the summary measure wouldn't be calculated properly.

# accessibility 0.1.0

- Initial CRAN release.
