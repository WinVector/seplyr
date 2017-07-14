
#' Standard interface for distinct.
#'
#' Group a data frame and add per-group indices as a column.
#'
#' @param .data data.frame
#' @param groupingVars character vector of column names to group by.
#' @param .keep_all logical, passed to dplyr::distinct.
#' @return .data passed through distinct with groupingVars args.
#'
#' @examples
#'
#' distinct_se(datasets::mtcars, c("cyl", "gear"))
#'
#' @export
#'
distinct_se <- function(.data, groupingVars, .keep_all = FALSE) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  distinct(.data = .data, !!!groupingSyms, .keep_all = .keep_all)
}
