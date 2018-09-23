
#' Simulate the group_by/mutate pattern with an explicit summarize and join.
#'
#' Group a data frame by the groupingVars and compute user summaries on
#' this data frame (user summaries specified in ...), then join these new
#' columns back into the original data and return to the user.
#' Author: John Mount, Win-Vector LLC.
#'
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param arrangeTerms character optional vector of column expressions to arrange by.
#' @param ... list of dplyr::mutate() expressions.
#' @return d with grouped summaries added as extra columns
#'
#' @examples
#'
#'
#' add_group_summaries(datasets::mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %.>%
#'   head(.)
#'
#' @export
#'
add_group_summaries <- function(d, groupingVars, ...,
                                arrangeTerms = NULL) {
  if(!(is.data.frame(d) || dplyr::is.tbl(d))) {
    stop("seplyr::add_group_summaries first argument must be a data.frame or tbl")
  }
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  d <- dplyr::ungroup(d) # just in case
  dg <- dplyr::group_by(d, !!!groupingSyms)
  if(length(arrangeTerms)>0) {
    # from: https://github.com/tidyverse/rlang/issues/116
    # now updated: https://github.com/WinVector/seplyr/issues/3
    env <- parent.frame()
    arrangeQ <- lapply(arrangeTerms,
                       function(si) {
                         rlang::parse_quo(si,
                                          env = env)
                       })
    dg <- dplyr::arrange(dg, !!!arrangeQ)
  }
  ds <- dplyr::summarize(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  ds <- dplyr::ungroup(ds)
  dplyr::left_join(d, ds, by= groupingVars)
}
