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


##### gravity ------------------------

library(accessibility)
library(data.table)
library(ggplot2)

vec <- 0:100
decay_value <- 0.5
cutoff <- 50

df <- data.table(
  minutes = vec,
  step = impedance_fun(t_ij=vec, decay_function='step', cutoff=cutoff),
  linear = impedance_fun(t_ij=vec, decay_function='linear', cutoff=cutoff),
  exponential = impedance_fun(t_ij=vec, decay_function='negative_exponential', decay_value = decay_value),
  inverse_power = impedance_fun(t_ij=vec, decay_function='inverse_power', decay_value = decay_value),
  modified_gaussian = impedance_fun(t_ij=vec, decay_function='modified_gaussian', decay_value = 50)
)

df2 <- data.table::melt.data.table(data = df, id.vars = 'minutes', variable.name = 'decay_function', value.name = 'impedance_factor')

ggplot() +
  geom_line(data=df2, aes(x=minutes, y=impedance_factor, color=decay_function), show.legend = FALSE) +
  facet_wrap(.~decay_function, ncol = 2)


ggsave('decay_functions.png', width = 10, height = 10, units = 'cm')



##### Coverage ------------------------
library(covr)
library(testthat)
Sys.setenv(NOT_CRAN = "true")


# each function separately

a <- covr::function_coverage(fun=accessibility::cumulative_time_cutoff, test_file("tests/testthat/test-cumulative_cutoff.R"))
a <- covr::function_coverage(fun=accessibility::cumulative_time_interval, test_file("tests/testthat/test-cumulative_interval.R"))
a <- covr::function_coverage(fun=accessibility::time_to_closest, test_file("tests/testthat/test-time_to_closest.R"))
a <- covr::function_coverage(fun=accessibility::impedance_fun, test_file("tests/testthat/test-impedance_fun.R"))
a <- covr::function_coverage(fun=accessibility::gravity_access, test_file("tests/testthat/test-gravity_access.R"))
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





