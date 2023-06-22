## Test environments

- Local Ubuntu 20.04 installation (R 4.2.1)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Windows Server 2022, R-devel, 64 bit
  - Fedora Linux, R-devel, clang, gfortran
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
- win-builder (devel, release, oldrel)

## R CMD check results

In most platforms we got the following result:

0 errors | 0 warnings | 0 notes

However, when using win-builder we got this result:

0 errors | 0 warnings | 1 note

Found the following (possibly) invalid file URIs:
  URI: 2020-12-07
    From: man/balancing_cost.Rd
  URI: 2023-06-15
    From: man/cumulative_interval.Rd
  URI: 2023-06-15
    From: man/fgt_poverty.Rd
  URI: 2022-07-14
    From: man/floating_catchment_area.Rd
  URI: 2023-02-16
    From: man/spatial_availability.Rd

We checked the mentioned files but could not find the possibly invalid URIs. 
