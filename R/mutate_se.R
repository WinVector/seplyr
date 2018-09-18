
#' mutate standard evaluation interface.
#'
#' Mutate a data frame by the mutateTerms.  Accepts arbitrary text as
#' mutateTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#' Semantics are: terms are evaluated left to right if splitTerms==TRUE (the default).
#'
#' Note: this method as the default setting \code{splitTerms = TRUE}, which is
#' safer (avoiding certain known \code{dplyr}/\code{dblyr} issues)
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
#' limit <- 3.5
#'
#' datasets::iris %.>%
#'   mutate_se(., qae(Sepal_Long = Sepal.Length >= 2 * Sepal.Width,
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
  wrapr::stop_if_dot_args(substitute(list(...)), "seplyr::mutate_se")
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
                          rlang::parse_quo(si,
                                           env = env)
                        })
      res <- dplyr::mutate(.data = res, !!!mutateQ)
    }
  }
  res
}

