## Test environments

- Local Ubuntu 20.04 installation (R 4.3.1)
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

In most platforms we got this result:

0 errors | 0 warnings | 0 notes

However, when using win-builder oldrel we got this:

0 errors | 0 warnings | 1 note

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1068/b29120
    From: inst/doc/accessibility.html
    Status: 403
    Message: Forbidden
  URL: https://doi.org/10.2307/1913475
    From: inst/doc/inequality_and_poverty.html
    Status: 403
    Message: Forbidden

We believe this note is a false positive, since we haven't had any problems
accessing the mentioned URLs.
