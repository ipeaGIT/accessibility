data_dir <- system.file("extdata", package = "accessibility")

travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
travel_matrix_list <- list(transit = travel_matrix, transit2 = travel_matrix)
travel_matrix <- data.table::rbindlist(travel_matrix_list, idcol = "mode")

land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))
