
#' Filter non-standard interface (deprecated).
#'
#' Filter a data frame by the filter terms in \code{...} (deprecated, please use \code{\link{filter_se}}).
#'
#' @seealso \code{\link{filter_se}}, \code{\link[dplyr]{filter}}, \code{\link[dplyr]{filter_at}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to filter by.
#' @param filter_nse_env environment to work in.
#' @return .data filtered by columns named in filterTerms
#'
#' @export
#'
filter_nse <- function(.data, ...,
                       filter_nse_env = parent.frame()) {
  .Deprecated(new = "filter_se", old = "filter_nse")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::filter_nse first argument must be a data.frame or tbl")
  }
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
      terms[[i-1]] <- paste(deparse(prep_deref(ei, filter_nse_env)), collapse = "\n")
    }
    res <- filter_se(res, terms, env=filter_nse_env)
  }
  res
}
