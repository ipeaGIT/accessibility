# location-allocation

library(accessibility)
library(data.table)




data_dir <- system.file("extdata", package = "accessibility")
travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))


df <- accessibility:::merge_by_reference(data = travel_matrix,land_use_data = land_use_data, active = T, opportunity = 'schools')
df <- accessibility:::merge_by_reference(data = df,land_use_data = land_use_data, active = F, opportunity = 'population')


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



head(df)

# location which minimizes average distance
df[, .(d = to_id[which.min(weighted.mean(travel_time, w=population))] )]

# location which minimizes average distance for places that take more then 20 min

df[ & , .(d = to_id[which.min(weighted.mean(travel_time, w=population))] )]




#' P-Median Problem
#' location which minimizes total distance
#' https://pysal.org/spopt/notebooks/p-median.html
df[, .(d = to_id[which.min(sum(travel_time * population))] )]
