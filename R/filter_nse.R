
#' Filter non-standard interface.
#'
#' Filter a data frame by the filter terms in \code{...}.
#'
#' @seealso \code{\link{filter_se}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{filter_at}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to filter by.
#' @param filter_nse_env environment to work in.
#' @return .data filtered by columns named in filterTerms
#'
#' @examples
#'
#' upperBound <- 3.5
#'
#' datasets::iris %.>%
#'   filter_nse(., Sepal.Length >= 2 * Sepal.Width,
#'                   Petal.Length <= upperBound)
#'
#' @export
#'
filter_nse <- function(.data, ...,
                       filter_nse_env = parent.frame()) {
  force(filter_nse_env)
  filterTerms <- wrapr::qe(...)
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::filter_nse first argument must be a data.frame or tbl")
  }
  len <- length(filterTerms)
  res <- .data
  if(len>1) {
    res <- filter_se(res, filterTerms, env=filter_nse_env)
  }
  res
}
