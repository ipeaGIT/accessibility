options(java.parameters = "-Xmx30G")

bhtrans_gtfs <- "../../data-raw/gtfs/bho/2019/gtfs_bho_bhtrans_2019-10.zip"
micro_gtfs <- "../../data-raw/gtfs/bho/2019/gtfs_bho_suplementar_2019-10.zip"
street_network <- "../../data-raw/malha_viaria/2019/bho/malha_viaria_2019-09_bho.pbf"
topo <- "../../data-raw/topografia/bho/topografia3_bho.tif"

if (!dir.exists("r5")) dir.create("r5")

if (!dir.exists("r5/bho")) dir.create("r5/bho")

file.copy(bhtrans_gtfs, "r5/bho/bhtrans.zip")
file.copy(micro_gtfs, "r5/bho/suplementar.zip")
file.copy(street_network, "r5/bho/street_network.pbf")
file.copy(topo, "r5/bho/topo.tif")

r5r_core <- r5r::setup_r5("r5/bho")
fare_structure <- r5r::setup_fare_structure(r5r_core, 5)
fare_structure$fares_per_transfer[, fare := 7.5]

grid_path <- system.file("extdata/grid_bho.rds", package = "accessibility")
grid <- readRDS(grid_path)

points <- sf::st_centroid(grid)

frontier <- r5r::pareto_frontier(
  r5r_core,
  origins = points,
  destinations = points,
  mode = c("WALK", "TRANSIT"),
  departure_datetime = as.POSIXct(
    "02-09-2019 14:00:00",
    format = "%d-%m-%Y %H:%M:%S"
  ),
  time_window = 1,
  max_walk_time = 30,
  max_trip_duration = 120,
  fare_structure = fare_structure,
  fare_cutoffs = c(0, 5, 7.5, 10, 12.5),
  max_rides = 3,
  progress = TRUE,
  n_threads = 30
)
frontier[, percentile := NULL]

saveRDS(frontier, "inst/extdata/pareto_frontier.rds", compress = "xz")
