



#' mutate non-standard evaluation interface (deprecated).
#'
#' Mutate a data frame by the mutate terms from \code{...} (deprecated, please use \code{\link{mutate_se}}).
#'
#' Note: this method as the default setting \code{mutate_nse_split_terms = TRUE}, which while
#' safer (avoiding certain known \code{dplyr}/\code{dblyr} issues) can be needlessly expensive
#' and have its own "too long sequence" issues on remote-data systems
#' (please see the side-notes of \url{http://winvector.github.io/FluidData/partition_mutate.html} for some references).
#'
#' @seealso \code{\link{mutate_se}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... expressions to mutate by.
#' @param mutate_nse_split_terms logical, if TRUE into separate mutates (if FALSE instead, pass all at once to dplyr).
#' @param mutate_nse_env environment to work in.
#' @param mutate_nse_printPlan logical, if TRUE print the expression plan
#' @return .data with altered columns.
#'
#' @export
#'
mutate_nse <- function(.data, ...,
                       mutate_nse_split_terms = TRUE,
                       mutate_nse_env = parent.frame(),
                       mutate_nse_printPlan = FALSE) {
  .Deprecated(new = "mutate_se", old = "mutate_nse")
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  mutateTerms <- substitute(list(...))
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::mutate_nse first argument must be a data.frame or tbl")
  }
  if(length(setdiff(names(mutateTerms), ""))>0) {
    stop("seplyr::mutate_nse() all assignments must be of the form a := b, not a = b")
  }
  # mutateTerms is a list of k+1 items, first is "list" the rest are captured expressions
  res <- .data
  len <- length(mutateTerms) # first slot is "list"
  if(len>1) {
    lhs <- vector(len-1, mode='list')
    rhs <- vector(len-1, mode='list')
    for(i in (2:len)) {
      ei <- mutateTerms[[i]]
      if((length(ei)!=3)||(as.character(ei[[1]])!=':=')) {
        stop("mutate_nse terms must be of the form: sym := expr")
      }
      lhs[[i-1]] <- as.character(prep_deref(ei[[2]], mutate_nse_env))
      rhs[[i-1]] <- paste(deparse(prep_deref(ei[[3]], mutate_nse_env)), collapse = "\n")
    }
    res <- mutate_se(res, lhs := rhs,
                     splitTerms = mutate_nse_split_terms,
                     env = mutate_nse_env,
                     printPlan = mutate_nse_printPlan)
  }
  res
}

