
#' mutate standard evaluation interface.
#'
#' Mutate a data frame by the mutateTerms.  Accepts arbitrary text as
#' mutateTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#' Semantics are: terms are evaluated left to right if splitTerms==TRUE (the default).
#'
#' Note: this method as the default setting \code{splitTerms = TRUE}, which while
#' safer (avoiding certain known \code{dplyr}/\code{dblyr} issues) can be needlessly expensive
#' and have its own "too long sequence" issues on remote-data systems
#' (please see the side-notes of \url{http://winvector.github.io/FluidData/partition_mutate.html} for some references).
#'
#' @seealso \code{\link{mutate_nse}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param mutateTerms character vector of column expressions to mutate by.
#' @param ... force later terms to be bound by name
#' @param splitTerms logical, if TRUE into separate mutates (if FALSE instead, pass all at once to dplyr).
#' @param env environment to work in.
#' @param printPlan logical, if TRUE print the expression plan
#' @return .data with altered columns.
#'
#' @examples
#'
#'
#' resCol1 <- "Sepal_Long"
#' limit <- 3.5
#'
#' datasets::iris %.>%
#'   mutate_se(., qae(resCol1 := Sepal.Length >= 2 * Sepal.Width,
#'                    Petal_Short := Petal.Length <= limit)) %.>%
#'   head(.)
#'
#'
#' @export
#'
mutate_se <- function(.data, mutateTerms,
                      ...,
                      splitTerms = TRUE,
                      env = parent.frame(),
                      printPlan = FALSE) {
  if(length(list(...))>0) {
    stop("seplyr::mutate_se unexpected arguments")
  }
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::mutate_nse first argument must be a data.frame or tbl")
  }
  res <- .data
  if(length(mutateTerms)>0) {
    if(splitTerms && (length(mutateTerms)>1)) {
      plan <- partition_mutate_se(mutateTerms)
      if(printPlan) {
        print(plan)
      }
      for(bi in plan) {
        res <- mutate_se(res, bi, splitTerms = FALSE, env = env)
      }
    } else {
      if(printPlan) {
        print(mutateTerms)
      }
      mutateQ <- lapply(mutateTerms,
                        function(si) {
                          rlang::parse_quosure(si,
                                               env = env)
                        })
      res <- dplyr::mutate(.data = res, !!!mutateQ)
    }
  }
  res
}

