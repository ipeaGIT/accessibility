#' @keywords internal
fill_missing_ids <- function(access_df, possible_combinations, groups) {
  filled_access_df <- merge(
    possible_combinations,
    access_df,
    by = groups,
    all.x = TRUE
  )
  filled_access_df[is.na(access), access := 0]

  return(filled_access_df[])
}
