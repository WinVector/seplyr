
#' arrange standard interface.
#'
#' Arrange a data frame by the arrangeTerms.  Accepts arbitrary text as
#' arrangeTerms to allow forms such as "desc(gear)"
#' (unlike \code{\link[dplyr]{arrange_at}}). Intent is to arrange only by
#' sets of variables and simple transforms, not by arbitrary expressions over variables.
#'
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{arrange_at}}
#'
#' @param .data data.frame
#' @param arrangeTerms character vector of column expressions to arrange by.
#' @return .data arrnaged by arrangeTerms
#'
#' @examples
#'
#' datasets::mtcars %.>%
#'   arrange_se(., c("cyl", "desc(gear)")) %.>%
#'   head(.)
#' # roughly equivilent to:
#' # arrange(datasets::mtcars, cyl, desc(gear)) %>% head()
#'
#' @export
#'
arrange_se <- function(.data, arrangeTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  env <- parent.frame()
  arrangeQ <- lapply(arrangeTerms,
                    function(si) {
                      rlang::parse_quosure(si,
                                           env = env)
                    })
  dplyr::arrange(.data = .data, !!!arrangeQ)
}
