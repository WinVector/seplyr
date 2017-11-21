
#' filter non-standard interface.
#'
#' Filter a data frame by the filterTerms.  Accepts arbitrary text as
#' filterTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
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
#'   filter_nse(., "Sepal.Length" >= 2 * "Sepal.Width",
#'                  "Petal.Length" <= upperBound) %.>%
#'   head(.)
#'
#'
#' @export
#'
filter_nse <- function(.data, ...,
                       filter_nse_env = parent.frame()) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  filterTerms <- substitute(list(...))
  if(!all(names(filterTerms) %in% "")) {
    stop("seplyr::filter_nse() unexpected names in '...'")
  }
  # filterTerms is a list of k+1 items, first is "list" the rest are captured expressions
  res <- .data
  len <- length(filterTerms)
  if(len>1) {
    terms <- vector(len-1, mode='list')
    for(i in (2:len)) {
      ei <- filterTerms[[i]]
      terms[[i-1]] <- deparse(prep_deref(ei, filter_nse_env))
    }
    res <- filter_se(res, terms, env=filter_nse_env)
  }
  res
}
