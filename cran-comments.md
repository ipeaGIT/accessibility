## Test environments

- Local Ubuntu 20.04 installation (R 4.2.1)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
- win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 2 notes

>  New maintainer:                                          
>    Daniel Herszenhut <dhersz@gmail.com>                                   
>  Old maintainer(s):         
>    Rafael H. M. Pereira <rafa.pereira.br@gmail.com>                       
>                                     
>  Found the following (possibly) invalid URLs:             
>    URL: https://doi.org/10.1068/b29120                 
>      From: inst/doc/accessibility.html
>      Status: 503                    
>      Message: Service Unavailable

We have decided to change maintainers in this version. The note regarding
the DOI seems to be a false positive, since we haven't had any problems
accessing the URL.
