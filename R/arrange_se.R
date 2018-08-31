
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
#' @param env environment to parse terms in.
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
arrange_se <- function(.data, arrangeTerms, env = parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::arrange_se first argument must be a data.frame or tbl")
  }
  if(length(arrangeTerms)>0) {
    arrangeQ <- lapply(arrangeTerms,
                       function(si) {
                         rlang::parse_quosure(si,
                                              env = env)
                       })
    .data <- dplyr::arrange(.data = .data, !!!arrangeQ)
  }
  .data
}
