
#' group_indices standard interface.
#'
#' Group a data frame by the groupingVars and add group labels.
#'
#' @seealso \code{\link[dplyr]{group_indices}}
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
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::group_indices_se first argument must be a data.frame or tbl")
  }
  if(!is.character(groupingVars)) {
    stop("seplyr::group_indices_se groupingVars must be a character vector")
  }
  if(!add) {
    .data <- dplyr::ungroup(.data)
  }
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  dplyr::group_indices(.data = .data, !!!groupingSyms)
}

