# accessibility

[![R-CMD-check](https://github.com/ipeaGIT/accessibility/workflows/R-CMD-check/badge.svg)](https://github.com/ipeaGIT/accessibility/actions) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) [![Codecov test coverage](https://codecov.io/gh/ipeaGIT/accessibility/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ipeaGIT/accessibility?branch=main)

**accessibility** offers a set of convenient functions to calculate multiple transport accessibility measures. Given a pre-computed travel costs (e.g. travel times, monetary costs) combined with land-use data (e.g. location of jobs, healthcare, population), the package will soon support various accessibility metrics such as: travel time cutoff- and interval-based cumulative opportunity (active and passive), minimum travel cost to closest N number of activities, gravitational measures and different floating catchment area methods.

# Installation:

`accessibility` is not available on CRAN yet. In the meantime, you may install the **development version**:

    # install.packages("remotes")
    remotes::install_github("ipeaGIT/accessibility")

## Related work:

-   [r5r](https://github.com/ipeaGIT/r5r): Rapid Realistic Routing with R5 in R
-   [tracc](https://github.com/jamaps/tracc): Transport accessibility measures in Python
-   [access](https://access.readthedocs.io/en/latest/): Spatial Access for PySAL


## Acknowledgement <a href="https://www.ipea.gov.br"><img src="man/figures/ipea_logo.png" alt="IPEA" align="right" width="300"/></a>

**accessibility** is developed by a team at the Institute for Applied Economic Research (Ipea), Brazil.
