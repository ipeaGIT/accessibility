
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
#> 1: 89a88cdb57bffff 22239
#> 2: 89a88cdb597ffff 36567
#> 3: 89a88cdb5b3ffff 42372
#> 4: 89a88cdb5cfffff 55571
#> 5: 89a88cd909bffff 26774
#> 6: 89a88cd90b7ffff 36991

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
#> 1: 89a88cdb57bffff 0.4357418
#> 2: 89a88cdb597ffff 0.3938630
#> 3: 89a88cdb5b3ffff 0.4589910
#> 4: 89a88cdb5cfffff 0.5469433
#> 5: 89a88cd909bffff 0.4358530
#> 6: 89a88cd90b7ffff 0.5271746
```

Please read the vignettes for more details on the usage:

  - Introduction to **accessibility**: calculating accessibility
    measures. Run
    [`vignette("accessibility")`](https://ipeagit.github.io/accessibility/articles/accessibility.html).
  - Decay functions. Run [`vignette("decay_functions", package =
    "accessibility")`](https://ipeagit.github.io/accessibility/articles/decay_functions.html).

## Related work:

  - [r5r](https://github.com/ipeaGIT/r5r): Rapid Realistic Routing with
    R5 in R
  - [tracc](https://github.com/jamaps/tracc): Transport accessibility
    measures in Python
  - [access](https://access.readthedocs.io/en/latest/): Spatial Access
    for PySAL

## Acknowledgement <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="IPEA" align="right" width="300"/></a>

**accessibility** is developed by a team at the Institute for Applied
Economic Research (Ipea), Brazil.
