# devtools::install_github("ipeaGIT/r5r", subdir = "r-package", force=T)
options(java.parameters = '-Xmx10G')
library(sf)
library(data.table)
library(magrittr)
library(roxygen2)
library(devtools)
library(usethis)
library(profvis)
library(dplyr)
library(mapview)
library(covr)
library(testthat)
library(ggplot2)
library(checkmate)
library(geobr)
library(gtfs2gps)
library(tictoc)
library(mapview)
mapviewOptions(platform = 'leafgl')


# utils::remove.packages('r5r')
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package", ref = 'detach_r5_codebase')
# library(r5r)




##### Coverage ------------------------
library(covr)
library(testthat)
library(accessibility)
Sys.setenv(NOT_CRAN = "true")


# each function separately

a <- covr::function_coverage(fun=accessibility::cumulative_time_cutoff, test_file("tests/testthat/test-cumulative_cutoff.R"))
a <- covr::function_coverage(fun=accessibility::cumulative_time_interval, test_file("tests/testthat/test-cumulative_interval.R"))
a

# the whole package
Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(path = ".", type = "tests")
cov



zeroCov <- covr::zero_coverage(a)


x <- as.data.frame(r5r_cov)



##### Profiling function ------------------------
# p <-   profvis( update_newstoptimes("T2-1@1#2146") )
#
# p <-   profvis( b <- corefun("T2-1") )





# checks spelling
library(spelling)
devtools::spell_check(pkg = ".", vignettes = TRUE, use_wordlist = TRUE)

# Update documentation
devtools::document(pkg = ".")


# Write package manual.pdf
system("R CMD Rd2pdf --title=Package gtfs2gps --output=./gtfs2gps/manual.pdf")
# system("R CMD Rd2pdf gtfs2gps")




1. Failure: detailed_itineraries output is correct (@test-detailed_itineraries.R#182)
2. Failure: detailed_itineraries output is correct (@test-detailed_itineraries.R#202)

pdflatex



path <- 'E:/Dropbox/other_projects/0_jean_capability/opentripplanner/otp_got'
options(java.parameters = "-Xmx16G")
r5r_core <- setup_r5(path)

### CMD Check ----------------
# Check package errors
library(tictoc)


# LOCAL
tictoc::tic()
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".",  cran = FALSE, env_vars = c(NOT_CRAN = "true"))
tictoc::toc()




# CRAN
tictoc::tic()
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
tictoc::toc()


devtools::check_win_release(pkg = ".")

# devtools::check_win_oldrelease()
# devtools::check_win_devel()


beepr::beep()



tictoc::tic()
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
tictoc::toc()





# build binary -----------------
system("R CMD build . --resave-data") # build tar.gz





