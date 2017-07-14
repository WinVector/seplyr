
#' arrange standard interface.
#'
#' Arange a data frame by the arrangeTerms.  Accepts arbitrary text as
#' arrangeTerms to allow forms such as "desc(gear)".
#' Author: John Mount, Win-Vector LLC.
#'
#' @param .data data.frame
#' @param arrangeTerms character vector of column expressions to group by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' arrange_se(datasets::mtcars, c("cyl", "desc(gear)")) %>%
#'   head()
#' # roughly equivilent to:
#' # arrange(datasets::mtcars, cyl, desc(gear)) %>% head()
#'
#' @export
#'
arrange_se <- function(.data, arrangeTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  arrangeTerms <- lapply(arrangeTerms,
                         function(si) { rlang::parse_expr(si) })
  arrange(.data = .data, !!!arrangeTerms)
}
