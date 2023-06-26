#' @param group_by A `character` vector. When not `character(0)` (the default),
#'   indicates the `accessibility_data` columns that should be used to group the
#'   inequality estimates by. For example, if `accessibility_data` includes a
#'   `scenario` column that identifies distinct scenarios that each
#'   accessibility estimates refer to (e.g. before and after a transport policy
#'   intervention), passing `"scenario"` to this parameter results in inequality
#'   estimates grouped by scenario.
