
#' group_by standard interface.
#'
#' Group a data frame by the groupingVars. group_by_se intentionally groups only by
#' sets of variables, not by expressions over variables.
#'
#' @seealso \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{group_by_at}}
#'
#' @param .data data.frame
#' @param groupingVars character vector of column names to group by.
#' @param add logical, passed to group_by
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#'
#' datasets::mtcars %.>%
#'   group_by_se(., c("cyl", "gear")) %.>%
#'   head(.)
#' # essentially group_by_at()
#'
#' @export
#'
group_by_se <- function(.data, groupingVars, add = FALSE) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  dplyr::group_by(.data = .data, !!!groupingSyms, add = add)
}

