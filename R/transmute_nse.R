
#' transmute non-standard evaluation interface.
#'
#' transmute a data frame by the transmuteTerms.  Accepts arbitrary text as
#' transmuteTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#'
#' @seealso \code{\link{transmute_se}}, \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{transmute_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to transmute by.
#' @param env environment to work in.
#' @return .data with altered columns(other columns dropped).
#'
#' @examples
#'
#'
#' resCol1 <- "Sepal_Long"
#' ratio <- 2
#' compCol1 <- "Sepal.Width"
#'
#'
#' datasets::iris %.>%
#'   transmute_nse(., resCol1 := "Sepal.Length" >= ratio * compCol1,
#'                 "Petal_Short" := "Petal.Length" <= 3.5) %.>%
#'   summary(.)
#'
#'
#' @export
#'
transmute_nse <- function(.data, ...,  env = parent.frame()) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  transmuteTerms <- substitute(list(...))
  # transmuteTerms is a list of k+1 items, first is "list" the rest are captured expressions
  res <- .data
  len <- length(transmuteTerms)
  if(len>1) {
    lhs <- vector(len-1, mode='list')
    rhs <- vector(len-1, mode='list')
    for(i in (2:len)) {
      ei <- transmuteTerms[[i]]
      if((length(ei)!=3)||(as.character(ei[[1]])!=':=')) {
        stop("transmute_nse terms must be of the form: sym := expr")
      }
      lhs[[i-1]] <- as.character(prep_deref(ei[[2]], env))
      rhs[[i-1]] <- deparse(prep_deref(ei[[3]], env))
    }
    res <- transmute_se(res, lhs := rhs, env=env)
  }
  res
}

