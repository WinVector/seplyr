
#' summarize non-standard evaluation interface.
#'
#' summarize a data frame by the summarizeTerms.  Accepts arbitrary text as
#' summarizeTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#'
#' @seealso  \code{\link{summarize_se}}, \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to summarize by.
#' @param env environment to work in.
#' @return .data with summarized columns.
#'
#' @examples
#'
#'
#' resCol <- "Sepal_Mean_Length"
#' varCol <- "Sepal.Length"
#'
#' datasets::iris %.>%
#'   group_by_se(., 'Species') %.>%
#'   summarize_nse(., resCol := mean(varCol),
#'                    "max_Sepal_Length" := max("Sepal.Length"))
#'
#'
#' @export
#'
summarize_nse <- function(.data, ..., env = parent.frame()) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  summarizeTerms <- substitute(list(...))
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
      rhs[[i-1]] <- deparse(prep_deref(ei[[3]], env))
    }
    res <- summarize_se(res, lhs := rhs, env=env)
  }
  res
}

#' @rdname summarize_nse
#' @export
summarise_nse <- summarize_nse
