
#' arrange standard interface.
#'
#' Arange a data frame by the arrangeTerms.  Accepts arbitrary text as
#' arrangeTerms to allow forms such as "desc(gear)"
#' (unlike \code{\link[dplyr]{arrange_at}}).
#'
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{arrange_at}}
#'
#' @param .data data.frame
#' @param arrangeTerms character vector of column expressions to arrange by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' datasets::mtcars %>%
#'   arrange_se(c("cyl", "desc(gear)")) %>%
#'   head()
#' # roughly equivilent to:
#' # arrange(datasets::mtcars, cyl, desc(gear)) %>% head()
#'
#' @export
#'
arrange_se <- function(.data, arrangeTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  arrangeQ <- lapply(arrangeTerms,
                     function(si) { rlang::parse_expr(si) })
  arrange(.data = .data, !!!arrangeQ)
}
