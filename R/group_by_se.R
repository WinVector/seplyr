
#' group_by standard interface.
#'
#' Group a data frame by the groupingVars.
#' Author: John Mount, Win-Vector LLC.
#'
#' @param .data data.frame
#' @param groupingVars character vector of column names to group by.
#' @param add logical, passed to group_by
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' group_by_se(datasets::mtcars, c("cyl", "gear")) %>%
#'   head()
#' # roughly equivalent to:
#' # do.call(group_by_, c(list(datasets::mtcars), c('cyl', 'gear')))
#'
#' @export
#'
group_by_se <- function(.data, groupingVars, add = FALSE) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  group_by(.data = .data, !!!groupingSyms, add = add)
}

