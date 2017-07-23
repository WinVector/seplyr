
#' Group a data frame and add per-group indices as a column.
#'
#' @param .data data.frame
#' @param groupingVars character vector of column names to group by.
#' @param indexColumn character name of column to add indices to.
#' @return .data with group identifying column added.
#'
#' @examples
#'
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' add_group_indices(datasets::mtcars, c("cyl", "gear"), 'groupID')
#'
#' @export
#'
add_group_indices <- function(.data, groupingVars, indexColumn) {
  .data <- dplyr::ungroup(.data) # just in case
  `:=` <- NULL # don't let look like an unbound reference to CRAN checker
  d <- dplyr::distinct(select_se(.data, groupingVars))
  d <- dplyr::mutate(d, !!indexColumn := 1 )
  d <- arrange_se(d, groupingVars)
  d <- dplyr::mutate(d, !!indexColumn := cumsum(!!rlang::sym(indexColumn)) )
  dplyr::left_join(.data, d, by = groupingVars)
}
