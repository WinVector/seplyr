
#' filter standard interface.
#'
#' Filter a data frame by the filterTerms.  Accepts arbitrary text as
#' filterTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#'
#' @seealso \code{\link[dplyr]{filter}}, \code{\link[dplyr]{filter_at}}
#'
#' @param .data data.frame
#' @param filterTerms character vector of column expressions to filter by.
#' @param env environment to work in.
#' @return .data filtered by columns named in filterTerms
#'
#' @examples
#'
#' upperBound <- 3.5
#'
#' datasets::iris %.>%
#'   filter_se(., qe(Sepal.Length >= 2 * Sepal.Width,
#'                   Petal.Length <= upperBound))
#'
#'
#' @export
#'
filter_se <- function(.data, filterTerms,  env=parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::filter_se first argument must be a data.frame or tbl")
  }
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  # updated: https://github.com/WinVector/seplyr/issues/3
  filterQ <- lapply(filterTerms,
                    function(si) {
                      rlang::parse_quo(si,
                                       env = env)
                    })
  dplyr::filter(.data = .data, !!!filterQ)
}
