
# accessibility: Transport Accessibility Metrics

<img align="right" src="man/figures/logo.png?raw=true" alt="logo" width="180">

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
pre-computed travel cost matrix in long format combined with land-use
data (e.g. location of jobs, healthcare, population), the package allows
one to calculate active and passive accessibility levels using multiple
accessibility metrics such as: cumulative opportunity measure (using
either travel time *cutoff* or *interval*), minimum travel cost to
closest *N* number of activities, gravitational measures and different
floating catchment area methods.

## Installation

`accessibility` is not available on CRAN yet. In the meantime, you may
install the **development version**:

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
 
# cumulative accessibility

df <- cumulative_cutoff(
  travel_matrix,
  land_use_data,
  cutoff = 30,
  opportunity_col = "jobs",
  travel_cost_col = "travel_time"
)
head(df)
#>                 id  jobs
#> 1: 89a88cdb57bffff 22239
#> 2: 89a88cdb597ffff 36567
#> 3: 89a88cdb5b3ffff 42372
#> 4: 89a88cdb5cfffff 55571
#> 5: 89a88cd909bffff 26774
#> 6: 89a88cd90b7ffff 36991

# gravity model

df <- gravity(
  travel_matrix,
  land_use_data,
  decay_function = decay_exponential(decay_value = 0.2),
  opportunity_col = "schools",
  travel_cost_col = "travel_time"
)
head(df)
#>                 id    schools
#> 1: 89a88cdb57bffff 0.03041853
#> 2: 89a88cdb597ffff 1.15549493
#> 3: 89a88cdb5b3ffff 0.56519126
#> 4: 89a88cdb5cfffff 0.19852152
#> 5: 89a88cd909bffff 0.41378042
#> 6: 89a88cd90b7ffff 0.95737555
                       
# 2SFCA with a binary (step) decay function

df <- floating_catchment_area(
  travel_matrix,
  land_use_data,
  fca_metric = "2sfca",
  decay_function = decay_binary(cutoff = 50),
  opportunity_col = "jobs",
  travel_cost_col = "travel_time",
  competition_col = "population"
)
head(df)
#>                 id      jobs
#> 1: 89a88cdb57bffff 0.4357418
#> 2: 89a88cdb597ffff 0.3938630
#> 3: 89a88cdb5b3ffff 0.4589910
#> 4: 89a88cdb5cfffff 0.5469433
#> 5: 89a88cd909bffff 0.4358530
#> 6: 89a88cd90b7ffff 0.5271746
```

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
