
#' Simulate the group_by/mutate pattern with an explicit summarize and join.
#'
#' Group a data frame by the groupingVars and compute user summaries on
#' this data frame (user summaries specified in ...), then join these new
#' columns back into the original data and return to the user.
#' Author: John Mount, Win-Vector LLC.
#'
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param arrangeTerms character vector of column expressions to group by.
#' @param ... list of dplyr::mutate() expressions.
#' @return d with grouped summaries added as extra columns
#'
#' @examples
#'
#' add_group_summaries(datasets::mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %>%
#'   head()
#'
#' @export
#'
add_group_summaries <- function(d, groupingVars, ...,
                                arrangeTerms = NULL) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  d <- ungroup(d) # just in case
  dg <- group_by(d, !!!groupingSyms)
  if(!is.null(arrangeTerms)) {
    # from: https://github.com/tidyverse/rlang/issues/116
    arrangeTerms <- lapply(arrangeTerms,
                           function(si) { rlang::parse_expr(si) })
    dg <- arrange(dg, !!!arrangeTerms)
  }
  ds <- summarize(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  ds <- ungroup(ds)
  left_join(d, ds, by= groupingVars)
}
