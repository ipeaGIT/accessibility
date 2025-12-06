https://maxcovr.njtierney.com/index.html
https://prioritizr.net/articles/publication_record.html
https://dirkschumacher.github.io/ompr/articles/problem-warehouse-location.html
https://github.com/njtierney/maxcovr

https://pysal.org/spopt/notebooks/p-median.html

# location-allocation brute force

library(accessibility)
library(data.table)
library(furrr)




data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))


df <- accessibility:::merge_by_reference(data = travel_matrix,
                                         land_use_data = land_use_data,
                                         left_df_idcol = 'from_id',
                                         opportunity = 'population')


df <- accessibility:::merge_by_reference(data = df,
                                         land_use_data = land_use_data,
                                         left_df_idcol = 'to_id'
                                         , opportunity = 'schools'
                                         )
head(df)




#' references
#'
#' Open-source approaches for location cover models: capabilities and efficiency
#' https://link.springer.com/article/10.1007/s10109-021-00350-w
#'
#' Mark green
#' https://github.com/markagreen/mapping_test_accessibility
#'
#' Pysal spopt
#' https://github.com/pysal/spopt


optimal_location <- function(df, candidates='all'){

  # candidates <- c('89a88cd90b7ffff', '89a88cdb607ffff')



  # ## drop origin areas with no population
  # df <- df[population>0,]

  ## get baseline time to access closest facility

  # find time closest
  # dfclosest <- df[,.(population=population[1L],
  #                    travel_to_closest=min(travel_time[which(schools>0)]),na.rm=TRUE),
  #                 by=from_id]

 # internal function to find travel time to closest facility
  get_closest <-function(ttmfull){
    temp_closest <- ttmfull[schools>0,.(population=population[1L],
                                travel_to_closest=min(travel_time,na.rm=TRUE)),
                    by=from_id]
    return(temp_closest)
  }

  # find travel time to closest facility
  dfclosest <- get_closest(ttmfull=df)

  # internal function to find average time to closest
  avg_time2closest <- function(df_closest){
    df_avg_time <- df_closest[population>0,.(wmean_time= weighted.mean(travel_to_closest, w=population, na.rm=TRUE))]
    return(df_avg_time)
    }

  # get baseline average time to closest
  baseline <- avg_time2closest(df_closest=dfclosest)$wmean_time

  # find all location candidates which do not have any facility
  # in a future version, the user can pass a vector of location candidates
  if(candidates=='all'){
    all_cadidates <- unique(df$to_id[which(df$schools==0)])
  }

  if (all(candidates!='all')) {
  all_cadidates = candidates
  }


  # function to simulate impact of allocation at an specific destination
  sim_one_location <- function(candn,
                               df = parent.frame()$df){

    # candn <- '89a88cd968fffff'

    # add 1 school to candn
    df[to_id == candn,]
    df[to_id == candn, schools := schools + 1]

    # find travel time to closest facility
    step1_closest <- get_closest(ttmfull = df)

    # get baseline average time to closest
    step1_avg_time <- avg_time2closest(df_closest = step1_closest)

    # add simulation id
    step1_avg_time[, candn := candn]
    return(step1_avg_time)
  }

  ## apply function to all candidate locations
  # df2 <- pbapply::pblapply(X=all_cadidates, FUN=sim_one_location, df=df) |>
  #        data.table::rbindlist()
  # parallel
  future::plan(strategy = 'multisession')
  df2 <- furrr::future_map(.x = all_cadidates,
                    .f = sim_one_location, .progress = TRUE, df=df) |>
    data.table::rbindlist()


  # location which minimizes average distance
  step2 <- df2[ wmean_time== min(wmean_time)]

  # organize output
  step2[, baseline_time := baseline]
  data.table::setnames(step2, old = 'candn', new = 'optimal_locations')
  data.table::setcolorder(step2, neworder = c('optimal_locations',
                                              'wmean_time',
                                              'baseline_time'))


  return(step2)
}


t <- optimal_location(df = df, candidates = 'all')




#' P-Median Problem
#' location which minimizes total distance
#' https://pysal.org/spopt/notebooks/p-median.html
df[, .(d = to_id[which.min(sum(travel_time * population))] )]
