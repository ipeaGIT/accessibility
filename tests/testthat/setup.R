data_dir <- system.file("extdata", package = "accessibility")

travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
travel_matrix_list <- list(transit = travel_matrix, transit2 = travel_matrix)
travel_matrix <- data.table::rbindlist(travel_matrix_list, idcol = "mode")

smaller_matrix <- travel_matrix[1:10]

small_matrix_wcost <- data.table::copy(smaller_matrix)
small_matrix_wcost[, monetary_cost := c(rep(c(0, 5, 10), 3), 5)]

land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

small_access <- cumulative_cutoff(
  smaller_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  cutoff = 30,
  group_by = "mode"
)
