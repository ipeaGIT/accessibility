


library(accessibility)

# load a travel time matrix data in long format
data_path <- system.file("extdata/ttm_bho.rds", package = "accessibility")
ttm <- readRDS(data_path)
ttm <- rbind(ttm, ttm, ttm, ttm, ttm)
ttm <- rbind(ttm, ttm, ttm, ttm, ttm)
ttm <- rbind(ttm, ttm, ttm, ttm, ttm)
nrow(ttm)

system.time(df <- gravity_access(data = ttm,
                                 opportunity_col = 'schools',
                                 decay_function = decay_exponential(decay_value = 0.2),
                                 travel_cost_col='travel_time',
                                 by_col = 'from_id'
))

system.time(
  df <- floating_catchment_area(
    data = ttm,
    fca_metric = '2SFCA',
    orig_col = 'from_id',
    dest_col = 'to_id',
    opportunity_col = 'jobs',
    population_col = 'population',
    decay_function = decay_binary(cutoff = 50),
    travel_cost_col = 'travel_time'
  )
  )
26.12



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




##### Coverage ------------------------
library(covr)
library(testthat)
Sys.setenv(NOT_CRAN = "true")


# the whole package
Sys.setenv(NOT_CRAN = "true")
cov <- covr::package_coverage(path = ".", type = "tests")
cov


x <- as.data.frame(cov)



# each function separately

a <- covr::function_coverage(fun=accessibility::cumulative_time_cutoff, test_file("tests/testthat/test-cumulative_cutoff.R"))
a <- covr::function_coverage(fun=accessibility::cumulative_time_interval, test_file("tests/testthat/test-cumulative_interval.R"))
a <- covr::function_coverage(fun=accessibility::time_to_closest, test_file("tests/testthat/test-time_to_closest.R"))
a <- covr::function_coverage(fun=accessibility::gravity_access, test_file("tests/testthat/test-gravity_access.R"))
a <- covr::function_coverage(fun=accessibility::floating_catchment_area, test_file("tests/testthat/test-floating_catchment_area.R"))
a <- covr::function_coverage(fun=accessibility::decay_binary, test_file("tests/testthat/test-decay_binary.R"))
a <- covr::function_coverage(fun=accessibility::decay_power, test_file("tests/testthat/test-decay_power.R"))
a <- covr::function_coverage(fun=accessibility::decay_exponential, test_file("tests/testthat/test-decay_exponential.R"))
a <- covr::function_coverage(fun=accessibility::decay_linear, test_file("tests/testthat/test-decay_linear.R"))
a

zeroCov <- covr::zero_coverage(a)




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

#
#
# # CRAN
# tictoc::tic()
# Sys.setenv(NOT_CRAN = "false")
# devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
# tictoc::toc()


devtools::check_win_release(pkg = ".")

# devtools::check_win_oldrelease()
# devtools::check_win_devel()


beepr::beep()



tictoc::tic()
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))
tictoc::toc()





# build binary -----------------
system("R CMD build . --resave-data") # build tar.gz




# submit to CRAN -----------------
# usethis::use_cran_comments('teste 2222, , asdadsad')


devtools::submit_cran()

