

# Generate data for inst -----------------------------------------------------

library(r5r)
library(data.table)

# build transport network
data_path <- system.file("extdata/poa", package = "r5r")
r5r_core <- setup_r5(data_path)

# load origin/destination points
points <- read.csv(file.path(data_path, "poa_hexgrid.csv"))[1:100,]

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
ttm[points, on=c('from_id'='id'), population   := i.population  ]
ttm[points, on=c('to_id'='id'), schools  := i.schools ]
ttm[points, on=c('to_id'='id'), healthcare  := i.healthcare ]


data.table::setnames(ttm, 'travel_time_p50' ,  'travel_time')

head(ttm)
nrow(ttm)

fwrite(ttm, file = 'ttm_poa.csv' )
