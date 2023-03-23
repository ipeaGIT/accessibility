apply_gravity_measure <- function(data, decay_function, travel_cost) {
  .cost_colname <- travel_cost

  # the decay function can come in two "formats"
  # - a function that returns a numeric vector
  # - a function that returns a list of numeric vectors
  # we have first to figure out which function we're dealing with, and then use
  # it accordingly

  decay_fn_output <- decay_function(1)

  is_list_length_1 <- is.list(decay_fn_output) && length(decay_fn_output) == 1

  if (is.numeric(decay_fn_output) || is_list_length_1) {
    data[, opp_weight := decay_function(get(.cost_colname))]

    access <- data
  } else {
    opp_weights <- decay_function(data[[travel_cost]])

    access <- lapply(
      opp_weights,
      function(opp_weight) cbind(data, opp_weight)
    )
    access <- data.table::rbindlist(access, idcol = "decay_function_arg")
    access[, decay_function_arg := as.numeric(decay_function_arg)]
  }

  return(access[])
}
