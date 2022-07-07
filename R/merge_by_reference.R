#' @keywords internal
merge_by_reference <- function(data,
                               land_use_data,
                               opportunity_col,
                               active) {
  # when calculating active accessibility, we want the number of opportunities
  # in the destination. if passive, the number of opportunities in the origin

  join_id <- ifelse(active, "to_id", "from_id")
  join_vector <- "id"
  names(join_vector) <- join_id

  data[
    land_use_data,
    on = join_vector,
    eval(opportunity_col) := get(paste0("i.", opportunity_col))
  ]
}
