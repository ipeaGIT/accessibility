
# accessibility: Transport Accessibility Metrics

<img align="right" src="man/figures/logo.png?raw=true" alt="logo" width="180">

[![CRAN
status](https://www.r-pkg.org/badges/version/accessibility)](https://CRAN.R-project.org/package=accessibility)
[![R-CMD-check](https://github.com/ipeaGIT/accessibility/workflows/R-CMD-check/badge.svg)](https://github.com/ipeaGIT/accessibility/actions)
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

# load a travel time matrix data in long format
data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
ttm <- readRDS(data_path)
 
# Cumulative accessibility
df <- cumulative_time_cutoff(
               data = ttm,
               cutoff = 30,
               opportunity_col = 'jobs',
               travel_cost_col = 'travel_time',
               by_col = 'from_id'
               )
df
#>              from_id access
#>   1: 89a88cdb57bffff  22239
#>   2: 89a88cdb597ffff  36567
#>   3: 89a88cdb5b3ffff  42372
#>   4: 89a88cdb5cfffff  55571
#>   5: 89a88cd909bffff  26774
#>  ---                       
#> 894: 89a881acda3ffff  30743
#> 895: 89a88cdb543ffff 140454
#> 896: 89a88cda667ffff  41845
#> 897: 89a88cd900fffff   5483
#> 898: 89a881aebafffff      0

# Gravity model
df <- gravity_access(data = ttm,
               opportunity_col = 'schools',
               decay_function = decay_exponential(decay_value = 0.2),
               travel_cost_col='travel_time',
               by_col = 'from_id'
               )
df
#>              from_id     access
#>   1: 89a88cdb57bffff 0.03041853
#>   2: 89a88cdb597ffff 1.15549493
#>   3: 89a88cdb5b3ffff 0.56519126
#>   4: 89a88cdb5cfffff 0.19852152
#>   5: 89a88cd909bffff 0.41378042
#>  ---                           
#> 894: 89a881acda3ffff 0.52732830
#> 895: 89a88cdb543ffff 0.18683580
#> 896: 89a88cda667ffff 0.81348400
#> 897: 89a88cd900fffff 0.08713560
#> 898: 89a881aebafffff 0.00000000
                       
# 2SFCA with a binary (step) decay function
df <- floating_catchment_area(
              data = ttm,
              fca_metric = '2SFCA',
              orig_col = 'from_id',
              dest_col = 'to_id',
              opportunity_col = 'jobs',
              population_col = 'population',
              decay_function = decay_binary(cutoff = 30),
              travel_cost_col='travel_time'
              )
df
#>              from_id access_2sfca
#>   1: 89a88cdb57bffff    0.1135253
#>   2: 89a88cdb597ffff    0.4412146
#>   3: 89a88cdb5b3ffff    0.4645848
#>   4: 89a88cdb5cfffff    0.3579043
#>   5: 89a88cd909bffff    0.2067682
#>  ---                             
#> 894: 89a881acda3ffff    0.1573290
#> 895: 89a88cdb543ffff    0.6269116
#> 896: 89a88cda667ffff    0.2801457
#> 897: 89a88cd900fffff    0.0740655
#> 898: 89a881aebafffff    0.0000000
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
