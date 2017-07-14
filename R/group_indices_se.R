
#' group_indices standard interface.
#'
#' Group a data frame by the groupingVars and add group labels.
#'
#' @param .data data.frame
#' @param groupingVars character vector of column names to group by.
#' @param add logical, passed to group_by
#' @return per-row group index assignments
#'
#' @examples
#'
#' group_indices_se(datasets::mtcars, c("cyl", "gear"))
#'
#' @export
#'
group_indices_se <- function(.data, groupingVars, add = FALSE) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  group_indices(.data = .data, !!!groupingSyms, add = add)
}

