#' @keywords internal
do_fill_missing_ids <- function(access_df,
                                possible_combinations,
                                groups,
                                access_col = "access",
                                fill_value = 0) {
  filled_access_df <- merge(
    possible_combinations,
    access_df,
    by = groups,
    all.x = TRUE
  )

  filled_access_df[
    is.na(get(access_col)),
    eval(access_col, envir = parent.frame()) := fill_value
  ]

  return(filled_access_df[])
}
