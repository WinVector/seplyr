
#' summarize non-standard evaluation interface (deprecated).
#'
#' summarize a data frame by the summarize terms from \code{...} (deprecated, please use \code{\link{summarize_se}}).
#'
#' @seealso  \code{\link{summarize_se}}, \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to summarize by.
#' @param env environment to work in.
#' @return .data with summarized columns.
#'
#'
#' @export
#'
summarize_nse <- function(.data, ..., env = parent.frame()) {
  .Deprecated(new = "summarize_se", old = "summarize_nse")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::summarize_nse first argument must be a data.frame or tbl")
  }
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  summarizeTerms <- substitute(list(...))
  if(!all(names(summarizeTerms) %in% "")) {
    stop("seplyr::summarize_nse() unexpected names in '...', all assignments must be of the form a := b, not a = b")
  }
  # summarizeTerms is a list of k+1 items, first is "list" the rest are captured expressions
  res <- .data
  len <- length(summarizeTerms)
  if(len>1) {
    lhs <- vector(len-1, mode='list')
    rhs <- vector(len-1, mode='list')
    for(i in (2:len)) {
      ei <- summarizeTerms[[i]]
      if((length(ei)!=3)||(as.character(ei[[1]])!=':=')) {
        stop("summarize_nse terms must be of the form: sym := expr")
      }
      lhs[[i-1]] <- as.character(prep_deref(ei[[2]], env))
      rhs[[i-1]] <- paste(deparse(prep_deref(ei[[3]], env)), collapse = "\n")
    }
    res <- summarize_se(res, lhs := rhs, env=env)
  }
  res
}

#' @rdname summarize_nse
#' @export
summarise_nse <- summarize_nse
