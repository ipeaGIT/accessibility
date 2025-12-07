# Changelog

## accessibility (development version)

### Bug fixes

### New features

### Notes

## accessibility 1.4.0

CRAN release: 2024-03-06

### Bug fixes

- Fixed a bug in which land use data passed to accessibility functions
  as a `tibble` could lead to an error.

### New features

- New function
  [`decay_logistic()`](https://ipeagit.github.io/accessibility/dev/reference/decay_logistic.md).

### Notes

- Package description has been adjusted to better reflect the current
  set of available features.

## accessibility 1.3.0

CRAN release: 2023-10-20

### New features

- New parameter `detailed_results` to
  [`spatial_availability()`](https://ipeagit.github.io/accessibility/dev/reference/spatial_availability.md),
  used to specify whether results should be aggregated by
  origin-destination pair or by origin. When aggregation by
  origin-destination pair, the output also includes the demand,
  impedance and combined balancing factors used to calculate spatial
  availability.

## accessibility 1.2.0

CRAN release: 2023-09-21

### New features

- New inequality functions
  [`concentration_index()`](https://ipeagit.github.io/accessibility/dev/reference/concentration_index.md)
  and
  [`theil_t()`](https://ipeagit.github.io/accessibility/dev/reference/theil_t.md).

### Notes

- Various documentation tweaks.
- Testing performance has greatly improved.

## accessibility 1.1.0

CRAN release: 2023-06-22

### New features

- The package now includes functions to estimate accessibility
  inequalities
  ([`palma_ratio()`](https://ipeagit.github.io/accessibility/dev/reference/palma_ratio.md)
  and
  [`gini_index()`](https://ipeagit.github.io/accessibility/dev/reference/gini_index.md))
  and poverty
  ([`fgt_poverty()`](https://ipeagit.github.io/accessibility/dev/reference/fgt_poverty.md)).
- New accessibility functions
  [`spatial_availability()`](https://ipeagit.github.io/accessibility/dev/reference/spatial_availability.md)
  and
  [`balancing_cost()`](https://ipeagit.github.io/accessibility/dev/reference/balancing_cost.md).
- New vignette “Calculating accessibility inequality and poverty”.
- [`cost_to_closest()`](https://ipeagit.github.io/accessibility/dev/reference/cost_to_closest.md)
  parameter `n` now accepts a numeric vector, instead of being
  restricted to a single number.
- [`cumulative_cutoff()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_cutoff.md)
  parameters `cutoff` and `travel_cost` now accepts a numeric and a
  character vector, respectively, instead of being restricted to a
  single number/string.
- [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md)
  parameter `interval` now accepts a list of numeric vectors, instead of
  being restricted to a single vector.
- The decay functions can now take numeric vectors as input, instead of
  being restricted to a single number (in the case of
  [`decay_stepped()`](https://ipeagit.github.io/accessibility/dev/reference/decay_stepped.md),
  both `steps` and `weights` can take a `list` of `numeric` vectors as
  input, instead of being restricted to a single `numeric` vector each).

### Notes

- Various documentation tweaks.
- Added the new accessibility functions to the intro vignette.

## accessibility 1.0.1

CRAN release: 2022-10-06

### Bug fixes

- Fixed a bug in which
  [`cost_to_closest()`](https://ipeagit.github.io/accessibility/dev/reference/cost_to_closest.md)
  would return `NA` values when filling missing ids (which should be
  filled with `Inf`, since they cannot reach any opportunities). This
  was also responsible for the warning reported in
  [\#27](https://github.com/ipeaGIT/accessibility/issues/27), which was
  also fixed.

## accessibility 1.0.0

CRAN release: 2022-07-22

The package has been to tremendous changes. Basically, there’s not a
single part of it that remained untouched: documentation, vignettes,
function names, parameter names, extra functionality, performance
improvements, etc. While it is impossible to highlight everything that
has been done, we’ll try to summary some of the key points in the
following topics.

### Breaking changes

- Accessibility functions previously worked with a single input dataset:
  `data`. Now they require two input datasets: `travel_matrix` and
  `land_use_data`.
- Function names were changed:
  - `time_to_closest()` -\>
    [`cost_to_closest()`](https://ipeagit.github.io/accessibility/dev/reference/cost_to_closest.md)
  - `cumulative_time_cutoff()` -\>
    [`cumulative_cutoff()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_cutoff.md)
  - `cumulative_time_interval()` -\>
    [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md)
  - `gravity_access()` -\>
    [`gravity()`](https://ipeagit.github.io/accessibility/dev/reference/gravity.md)
- Parameter names were changed:
  - `opportunity_col` -\> `opportunity`
  - `travel_cost_col` -\> `travel_cost`
  - `by_col` -\> `active`
  - In
    [`cost_to_closest()`](https://ipeagit.github.io/accessibility/dev/reference/cost_to_closest.md):
    `n_opportunities` -\> `n`
  - In
    [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md):
    `stat` -\> `summary_function`
  - In
    [`floating_catchment_area()`](https://ipeagit.github.io/accessibility/dev/reference/floating_catchment_area.md):
    `population_col` -\> `demand`
  - In
    [`floating_catchment_area()`](https://ipeagit.github.io/accessibility/dev/reference/floating_catchment_area.md):
    `fca_metric` -\> `method`
- Parameter required values were changed:
  - `active` now takes a `logical`, instead of a string (which `by_col`
    previously took).
  - In
    [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md):
    `summary_function` now takes a `function`, instead of a string
    (which `stat` previously took).

### New features

- New function
  [`decay_stepped()`](https://ipeagit.github.io/accessibility/dev/reference/decay_stepped.md).
- New parameter `interval_increment` to
  [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md),
  used to specify how many travel cost units separate the cutoffs used
  to calculate the accessibility estimates which will be used to
  calculate the summary estimate within the specified interval.
- All accessibility functions gained a `group_by` parameter, that allows
  accessibility estimates to be grouped by one or more columns present
  in `travel_matrix`.
- All accessibility functions (but
  [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md))
  gained a `fill_missing_ids` parameter, that includes in the results
  origins whose accessibility would be 0 but, due to some commonly
  overlooked implementation details, are usually left out from the
  output.
  [`cumulative_interval()`](https://ipeagit.github.io/accessibility/dev/reference/cumulative_interval.md)
  doesn’t have this parameter because its result will always include all
  origins, otherwise the summary measure wouldn’t be calculated
  properly.

## accessibility 0.1.0

CRAN release: 2022-06-30

- Initial CRAN release.
