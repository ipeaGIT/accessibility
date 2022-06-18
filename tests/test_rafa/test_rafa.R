##### example map ------------------------
library(accessibility)
library(data.table)
library(ggplot2)
library(sf)


ttm_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
grid_path <- system.file("extdata/grid_bho.rds", package = "accessibility")
ttm <- readRDS(ttm_path)
grid <- readRDS(grid_path)

setdiff(ttm$from_id, grid$id)

unique(ttm$from_id) |> length()
unique(grid$id) |> length()

# Active accessibility: number of schools accessible from each origin
df <- cumulative_time_cutoff(data = ttm,
                             opportunity_colname = 'jobs',
                             cutoff = 30,
                             by_colname = 'from_id')

df <- accessibility::gravity_access(data = ttm,
                             opportunity_colname = 'jobs',
                             decay_function = 'inverse_power',
                             decay_value = .5,
                             by_colname = 'from_id')

# access
df2 <- df[setDT(grid), on=c('from_id'='id'), geom := i.geom]

df2 <- st_sf(df2)
ggplot() +
  geom_sf(data=df2, aes(fill=access), color=NA) +
  scale_fill_viridis_c()



# isochone
id <- ttm[, .(t_min = mean(travel_time)), by = 'from_id']
id <- id[min(t_min)]$from_id
df <- ttm[ from_id ==id, ]
df2 <- df[setDT(grid), on=c('to_id'='id'), geom := i.geom]


df2 <- st_sf(df2)
ggplot() +
  geom_sf(data=df2, aes(fill=travel_time), color=NA) +
  scale_fill_viridis_c()


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





