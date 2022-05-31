

# Generate data for reprex -----------------------------------------------------

library(r5r)

# build transport network
data_path <- system.file("extdata/poa", package = "r5r")
r5r_core <- setup_r5(data_path)

# load origin/destination points
points <- read.csv(file.path(data_path, "poa_hexgrid.csv"))[1:200,]

departure_datetime <- as.POSIXct(
  "13-05-2019 14:00:00",
  format = "%d-%m-%Y %H:%M:%S"
)

ttm <- travel_time_matrix(
  r5r_core,
  origins = points,
  destinations = points,
  mode = 'transit',
  departure_datetime = departure_datetime
)

# merge opportunities data
df <- ttm[points, on=c('to_id'='id'), schools  := i.schools ]
head(df)



# Cumulative threshold -----------------------------------------------------

cumulative_time_threshold <- function(data, cutoff = 20, by_col='from_id'){

  access <- data[travel_time_p50 <= cutoff, .(access = sum(schools)), by=by_col]
  return(access)
}

cumulative_time_threshold(data = df, cutoff = 30, by_col='from_id')



# Cumulative interval -----------------------------------------------------

cumulative_time_interval <- function(data = df, start=20, end=30, by_col='from_id', summary='mean'){

  # minute-by-minute interval
  vct <- seq(start, end, 1)

  # calculate cumulative access for every minute in the interval
  access_list <- lapply(X=vct,
                        FUN= function(i){
                          temp <-  cumulative_time_threshold(data = df,
                                                             cutoff = i,
                                                             by_col=by_col)
                          return(temp)
                          }
                        )
  access <- data.table::rbindlist(access_list)

  # summary measure of choice
  if (summary=='mean') {
    access <- access[, mean(access, na.rm=T), by=from_id ] }

  if (summary=='median') {
    access <- access[, median(access, na.rm=T), by=from_id ] }

  return(access)
  }


cumulative_time_interval(data = df,
                         start=40,
                         end=60,
                         by_col='from_id',
                         summary='median')

