
#' group_by and summarize as an atomic action.
#'
#' Group a data frame by the groupingVars and compute user summaries on
#' this data frame (user summaries specified in ...).  Enforces the
#' good dplyr pipeline design principle of keeping group_by and
#' summarize close together.
#' Author: John Mount, Win-Vector LLC.
#'
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param ... list of dplyr::mutate() expressions.
#' @param arrangeTerms character optional vector of column expressions to arrange by.
#' @return d summarized by groups
#'
#' @examples
#'
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' group_summarize(datasets::mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %>%
#'   head()
#'
#' @export
#'
group_summarize <- function(d, groupingVars, ...,
                            arrangeTerms = NULL) {
  # convert char vector into spliceable vector
  groupingSyms <- rlang::syms(groupingVars)
  d <- dplyr::ungroup(d) # just in case
  dg <- dplyr::group_by(d, !!!groupingSyms)
  if(!is.null(arrangeTerms)) {
    # from: https://github.com/tidyverse/rlang/issues/116
    env <- parent.frame()
    arrangeQ <- lapply(arrangeTerms,
                      function(si) {
                        rlang::parse_quosure(si,
                                             env = env)
                      })
    dg <- dplyr::arrange(dg, !!!arrangeQ)
  }
  ds <- dplyr::summarize(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  dplyr::ungroup(ds)
}


#' @rdname group_summarize
#' @export
group_summarise <- group_summarize
