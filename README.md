
# accessibility

<img align="right" src="man/figures/logo.png" alt="logo" width="180">

[![CRAN
status](https://www.r-pkg.org/badges/version/accessibility)](https://CRAN.R-project.org/package=accessibility)
[![rcmdcheck](https://github.com/ipeaGIT/accessibility/workflows/rcmdcheck/badge.svg)](https://github.com/ipeaGIT/accessibility/actions)
[![CRAN/METACRAN Total
downloads](http://cranlogs.r-pkg.org/badges/grand-total/accessibility?color=yellow)](https://CRAN.R-project.org/package=accessibility)
[![Codecov test
coverage](https://codecov.io/gh/ipeaGIT/accessibility/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ipeaGIT/accessibility?branch=main)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

**accessibility** offers a set of fast and convenient functions to
calculate multiple transport accessibility measures. Given a
pre-computed travel cost matrix and a land use dataset (containing the
location of jobs, healthcare and population, for example), the package
allows one to calculate active and passive accessibility levels using
multiple accessibility measures, such as: cumulative opportunities
(using either travel cost cutoffs or intervals), minimum travel cost to
closest N number of activities, gravity-based (with different decay
functions) and different floating catchment area methods.

## Installation

Stable version:

``` r
install.packages("accessibility")
```

Development version:

``` r
# install.packages("remotes")
remotes::install_github("ipeaGIT/accessibility")
```

## Basic usage

``` r
library(accessibility)

# required data: a travel matrix and some land use data
data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

cost_closest <- cost_to_closest(
  travel_matrix,
  land_use_data,
  opportunity = "schools",
  travel_cost = "travel_time",
  n = 1
)
head(cost_closest)
#>                 id travel_time
#> 1: 89a881a5a2bffff          29
#> 2: 89a881a5a2fffff          24
#> 3: 89a881a5a67ffff          28
#> 4: 89a881a5a6bffff          33
#> 5: 89a881a5a6fffff          32
#> 6: 89a881a5b03ffff          17

cum_cutoff <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  cutoff = 30
)
head(cum_cutoff)
#>                 id  jobs
#> 1: 89a881a5a2bffff 14561
#> 2: 89a881a5a2fffff 29452
#> 3: 89a881a5a67ffff 16647
#> 4: 89a881a5a6bffff 10700
#> 5: 89a881a5a6fffff  6669
#> 6: 89a881a5b03ffff 37029

cum_interval <- cumulative_interval(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  interval = c(20, 30)
)
head(cum_interval)
#>                 id  jobs
#> 1: 89a88cdb57bffff  7649
#> 2: 89a88cdb597ffff 21990
#> 3: 89a88cdb5b3ffff 22282
#> 4: 89a88cdb5cfffff 26841
#> 5: 89a88cd909bffff 14421
#> 6: 89a88cd90b7ffff 25699

grav <- gravity(
  travel_matrix,
  land_use_data,
  opportunity = "schools",
  travel_cost = "travel_time",
  decay_function = decay_exponential(decay_value = 0.2)
)
head(grav)
#>                 id    schools
#> 1: 89a88cdb57bffff 0.03041853
#> 2: 89a88cdb597ffff 1.15549493
#> 3: 89a88cdb5b3ffff 0.56519126
#> 4: 89a88cdb5cfffff 0.19852152
#> 5: 89a88cd909bffff 0.41378042
#> 6: 89a88cd90b7ffff 0.95737555

fca <- floating_catchment_area(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  demand = "population",
  method = "2sfca",
  decay_function = decay_binary(cutoff = 50)
)
head(fca)
#>                 id      jobs
#> 1: 89a88cdb57bffff 0.4278111
#> 2: 89a88cdb597ffff 0.3863614
#> 3: 89a88cdb5b3ffff 0.4501725
#> 4: 89a88cdb5cfffff 0.5366707
#> 5: 89a88cd909bffff 0.4280401
#> 6: 89a88cd90b7ffff 0.5176583

sptl_avlblt <- spatial_availability(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  demand = "population",
  decay_function = decay_exponential(decay_value = 0.1)
)
head(sptl_avlblt)
#>                 id     jobs
#> 1: 89a88cdb57bffff 186.0876
#> 2: 89a88cdb597ffff 140.0738
#> 3: 89a88cdb5b3ffff 736.5830
#> 4: 89a88cdb5cfffff 900.9284
#> 5: 89a88cd909bffff   0.0000
#> 6: 89a88cd90b7ffff 204.7962

bc <- balancing_cost(
  travel_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  demand = "population"
)
head(bc)
#>                 id travel_time
#> 1: 89a881a5a2bffff          15
#> 2: 89a881a5a2fffff          13
#> 3: 89a881a5a67ffff          23
#> 4: 89a881a5a6bffff           7
#> 5: 89a881a5a6fffff          10
#> 6: 89a881a5b03ffff           6
```

Please read the vignettes for more details on the usage:

  - Introduction to **accessibility**: calculating accessibility
    measures. Run
    [`vignette("accessibility")`](https://ipeagit.github.io/accessibility/articles/accessibility.html).
  - Decay functions. Run [`vignette("decay_functions", package =
    "accessibility")`](https://ipeagit.github.io/accessibility/articles/decay_functions.html).
  - Calculating accessibility inequality and poverty. Run
    [`vignette("inequality_and_poverty", package =
    "accessibility")`](https://ipeagit.github.io/accessibility/articles/inequality_and_poverty.html).

## Related work:

  - [r5r](https://github.com/ipeaGIT/r5r): Rapid Realistic Routing with
    R5 in R
  - [tracc](https://github.com/jamaps/tracc): Transport accessibility
    measures in Python
  - [access](https://access.readthedocs.io/en/latest/): Spatial Access
    for PySAL
  - [aceso](https://github.com/tetraptych/aceso): a lightweight Python
    package for measuring spatial accessibility

## Acknowledgement <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="IPEA" align="right" width="300"/></a>

**accessibility** is developed by a team at the Institute for Applied
Economic Research (Ipea), Brazil.
