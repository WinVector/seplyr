
#' group_by and transmute as an atomic action.
#'
#' Group a data frame by the groupingVars and compute user summaries on
#' this data frame (user summaries specified in ...).  Enforces the
#' good dplyr pipeline design principle of keeping group_by and
#' transmute close together.
#' Author: John Mount, Win-Vector LLC.
#'
#' @param d data.frame
#' @param groupingVars character vector of column names to group by.
#' @param ... list of dplyr::transmute() expressions.
#' @param arrangeTerms character optional vector of quoted column expressions to arrange by.
#' @param env environment to work in.
#' @return d transmuteed by groups
#'
#' @examples
#'
#'
#' group_transmute(datasets::mtcars,
#'                     c("cyl", "gear"),
#'                     group_mean_mpg = mean(mpg),
#'                     group_mean_disp = mean(disp)) %.>%
#'   head(.)
#'
#' @export
#'
group_transmute <- function(d, groupingVars, ...,
                            arrangeTerms = NULL,
                            env = parent.frame()) {
  if(!(is.data.frame(d) || dplyr::is.tbl(d))) {
    stop("seplyr::group_transmute first argument must be a data.frame or tbl")
  }
  force(env)
  d <- dplyr::ungroup(d) # just in case
  if(length(groupingVars)>0) {
    if(!is.character(groupingVars)) {
      stop("seplyr::group_transmute groupingVars must be a character vector")
    }
    # convert char vector into spliceable vector
    groupingSyms <- rlang::syms(groupingVars)
    dg <- dplyr::group_by(d, !!!groupingSyms)
  } else {
    dg <- d
  }
  if(length(arrangeTerms)>0) {
    # from: https://github.com/tidyverse/rlang/issues/116
    # updated: https://github.com/WinVector/seplyr/issues/3
    if(!is.character(arrangeTerms)) {
      stop("seplyr::group_transmute arrangeTerms must be length 0, or character")
    }
    arrangeQ <- lapply(arrangeTerms,
                       function(si) {
                         rlang::parse_quo(si,
                                          env = env)
                       })
    dg <- dplyr::arrange(dg, !!!arrangeQ)
  }
  ds <- dplyr::transmute(dg, ...)
  # work around https://github.com/tidyverse/dplyr/issues/2963
  dplyr::ungroup(ds)
}

