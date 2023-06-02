data_dir <- system.file("extdata", package = "accessibility")

travel_matrix <- readRDS(file.path(data_dir, "travel_matrix.rds"))
travel_matrix_list <- list(transit = travel_matrix, transit2 = travel_matrix)
travel_matrix <- data.table::rbindlist(travel_matrix_list, idcol = "mode")

smaller_matrix <- travel_matrix[1:10]

land_use_data <- readRDS(file.path(data_dir, "land_use_data.rds"))

small_access <- cumulative_cutoff(
  smaller_matrix,
  land_use_data,
  opportunity = "jobs",
  travel_cost = "travel_time",
  cutoff = 30,
  group_by = "mode"
)

pareto_frontier <- readRDS(file.path(data_dir, "pareto_frontier.rds"))
frontier_list <- list(transit = pareto_frontier, transit2 = pareto_frontier)
pareto_frontier <- data.table::rbindlist(frontier_list, idcol = "mode")

small_frontier <- pareto_frontier[1:10]
