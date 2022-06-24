# accessibility <img align="right" src="man/figures/logo.png?raw=true" alt="logo" width="180">

[![R-CMD-check](https://github.com/ipeaGIT/accessibility/workflows/R-CMD-check/badge.svg)](https://github.com/ipeaGIT/accessibility/actions) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![Codecov test coverage](https://codecov.io/gh/ipeaGIT/accessibility/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ipeaGIT/accessibility?branch=main)

**accessibility** offers a set of fast and convenient functions to calculate multiple transport accessibility measures. Given a pre-computed travel cost matrix in long format combined with land-use data (e.g. location of jobs, healthcare, population), the package allows one to calculate active and passive accessibility levels using multiple accessibility metrics such as: cumulative opportunity measure (using either travel time *cutoff* or *interval*), minimum travel cost to closest *N* number of activities, gravitational measures and different floating catchment area methods.

# Installation:

`accessibility` is not available on CRAN yet. In the meantime, you may install the **development version**:

    # install.packages("remotes")
    remotes::install_github("ipeaGIT/accessibility")


# Basic usage

```
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

# Gravity model
df <- gravity_access(data = ttm,
               opportunity_col = 'schools',
               decay_function = decay_exponential(decay_value = 0.2),
               travel_cost_col='travel_time',
               by_col = 'from_id'
               )
                       
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
```

## Related work:

-   [r5r](https://github.com/ipeaGIT/r5r): Rapid Realistic Routing with R5 in R
-   [tracc](https://github.com/jamaps/tracc): Transport accessibility measures in Python
-   [access](https://access.readthedocs.io/en/latest/): Spatial Access for PySAL


## Acknowledgement <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="IPEA" align="right" width="300"/></a>

**accessibility** is developed by a team at the Institute for Applied Economic Research (Ipea), Brazil.
