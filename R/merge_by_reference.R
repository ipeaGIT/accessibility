#' @keywords internal
merge_by_reference <- function(data,
                               land_use_data,
                               join_vector,
                               opportunity_col) {
  data[
    land_use_data,
    on = join_vector,
    eval(opportunity_col) := get(paste0("i.", opportunity_col))
  ]
}
