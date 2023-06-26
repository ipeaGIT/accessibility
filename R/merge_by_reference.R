#' @keywords internal
merge_by_reference <- function(data,
                               land_use_data,
                               opportunity,
                               left_df_idcol) {
  # when calculating active accessibility, we want the number of opportunities
  # in the destination. if passive, the number of opportunities in the origin

  right_df_idcol <- "id"

  join_vector <- right_df_idcol
  names(join_vector) <- left_df_idcol

  data[
    land_use_data,
    on = join_vector,
    eval(opportunity) := get(paste0("i.", ..opportunity))
  ]
}
