library(data.table)
library(lpSolve)
library(accessibility)

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


names(df) <- c('origin_id',
               'destination_id',
               'travel_time',
               'origin_population',
               'destination_facilities')

rowpos <- sample(1:nrow(df), size = 500, replace = T) |> sort()
data <- df[rowpos, ]

result <- solve_p_median(df, p = 2)

######################
data <- data.table(
  origin_id = rep(1:3, each = 3),
  destination_id = rep(1:3, times = 3),
  travel_time = c(10, 20, 30, 15, 25, 35, 20, 30, 40),
  origin_population = rep(c(100, 150, 200), each = 3),
  destination_facilities = rep(0, 9)  # Assuming no facilities are open yet
)

# Solve the p-Median Problem for p = 2
result <- solve_p_median(data, p = 2)

# View the results
print(result$total_cost)
print(result$assignment)
print(result$facilities_opened)


solve_p_median <- function(data, p) {
  # Ensure data is a data.table
  if (!is.data.table(data)) {
    stop("Input data must be a data.table.")
  }

  # Check required columns
  required_cols <- c("origin_id", "destination_id", "travel_time",
                     "origin_population", "destination_facilities")
  if (!all(required_cols %in% names(data))) {
    stop("Data must contain the following columns: ", paste(required_cols, collapse = ", "))
  }

  # Get unique origins and destinations
  origins <- unique(data$origin_id)
  destinations <- unique(data$destination_id)

  # Number of origins and destinations
  n_origins <- length(origins)
  n_destinations <- length(destinations)

  # Create index mappings
  origin_index <- setNames(1:n_origins, origins)
  destination_index <- setNames(1:n_destinations, destinations)

  # Demand at each origin
  demand <- data[, .(origin_population = mean(origin_population)), by = origin_id]
  demand <- demand[order(origin_id)]
  demand_vector <- demand$origin_population

  # Travel time matrix (origins x destinations)
  travel_time_matrix <- matrix(0, nrow = n_origins, ncol = n_destinations)
  for (i in 1:nrow(data)) {
    o <- origin_index[as.character(data$origin_id[i])]
    d <- destination_index[as.character(data$destination_id[i])]
    travel_time_matrix[o, d] <- data$travel_time[i]
  }

  # Decision variables:
  # x[i, j] = 1 if origin i is assigned to facility at destination j
  # y[j] = 1 if a facility is located at destination j

  # Objective: Minimize total weighted travel time
  # Minimize sum_{i in origins} sum_{j in destinations} demand[i] * travel_time[i, j] * x[i, j]

  # Constraints:
  # 1. Each origin is assigned to exactly one facility: sum_{j} x[i, j] = 1 for all i
  # 2. Assignment only to open facilities: x[i, j] <= y[j] for all i, j
  # 3. Number of facilities to open: sum_{j} y[j] = p
  # 4. y[j] are binary variables
  # 5. x[i, j] are binary variables

  # Number of variables:
  # Total x[i, j]: n_origins * n_destinations
  # Total y[j]: n_destinations
  total_vars <- n_origins * n_destinations + n_destinations

  # Objective coefficients
  obj_coeffs <- c(as.vector(travel_time_matrix * demand_vector), rep(0, n_destinations))

  # Variable types: binary
  var_types <- rep("binary", total_vars)

  # Constraints
  constraints <- list()
  constr_rhs <- c()
  constr_dir <- c()

  # Constraint 1: Each origin is assigned to exactly one facility
  for (i in 1:n_origins) {
    constr <- rep(0, total_vars)
    constr[((i - 1) * n_destinations + 1):(i * n_destinations)] <- 1
    constraints <- c(constraints, list(constr))
    constr_rhs <- c(constr_rhs, 1)
    constr_dir <- c(constr_dir, "==")
  }

  # Constraint 2: Assignment only to open facilities x[i, j] - y[j] <= 0
  for (i in 1:n_origins) {
    for (j in 1:n_destinations) {
      constr <- rep(0, total_vars)
      constr[(i - 1) * n_destinations + j] <- 1  # x[i, j]
      constr[n_origins * n_destinations + j] <- -1  # -y[j]
      constraints <- c(constraints, list(constr))
      constr_rhs <- c(constr_rhs, 0)
      constr_dir <- c(constr_dir, "<=")
    }
  }

  # Constraint 3: Number of facilities to open
  constr <- rep(0, total_vars)
  constr[(n_origins * n_destinations + 1):total_vars] <- 1  # y[j]
  constraints <- c(constraints, list(constr))
  constr_rhs <- c(constr_rhs, p)
  constr_dir <- c(constr_dir, "==")

  # Combine constraints
  constraint_matrix <- do.call(rbind, constraints)

  # Solve the problem
  result <- lp("min",
               objective.in = obj_coeffs,
               const.mat = constraint_matrix,
               const.dir = constr_dir,
               const.rhs = constr_rhs,
               all.bin = TRUE)

  if (result$status != 0) {
    stop("Optimal solution not found.")
  }

  # Extract the solution
  solution <- result$solution
  x_vars <- matrix(solution[1:(n_origins * n_destinations)], nrow = n_origins, byrow = TRUE)
  y_vars <- solution[(n_origins * n_destinations + 1):total_vars]

  # Prepare assignment results
  assignment <- data.table(
    origin_id = origins,
    assigned_destination_id = destinations[max.col(x_vars, ties.method = "first")]
  )

  # Facilities opened
  facilities_opened <- data.table(
    destination_id = destinations,
    facility_opened = as.logical(y_vars)
  )

  # Return results
  list(
    total_cost = result$objval,
    assignment = assignment,
    facilities_opened = facilities_opened
  )
}
