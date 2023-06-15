bho_grid <- readRDS(
  system.file("extdata/grid_bho.rds", package = "accessibility")
)

bho_data <- aopdata::read_landuse("bho")

bho_data <- bho_data[
  ,
  .(
    id = id_hex,
    population = P001,
    jobs = T001,
    schools = E001,
    income_per_capita = R001
  )
]
bho_data <- bho_data[id %in% bho_grid$id]

land_use_path <- system.file(
  "extdata/land_use_data.rds",
  package = "accessibility"
)
saveRDS(bho_data, land_use_path)
