
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
#' @param warn logical, if TRUE warn about name re-use.
#' @param env environment to work in.
#' @param printPlan logical, if TRUE print the expression plan.
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
                      warn = TRUE,
                      env = parent.frame(),
                      printPlan = FALSE) {
  force(env)
  wrapr::stop_if_dot_args(substitute(list(...)), "seplyr::mutate_se")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::mutate_nse first argument must be a data.frame or tbl")
  }
  if(!check_is_char_vec_or_listscal(mutateTerms)) {
    stop("seplyr::mutate_se mutateTerms must be a character vector or list of character scalars")
  }
  res <- .data
  if(length(mutateTerms)>0) {
    plan <- NULL
    if(splitTerms || warn) {
      plan <- partition_mutate_se(mutateTerms)
      if(printPlan) {
        print(plan)
      }
      if(warn && (length(plan)>1)) {
        warning("seplyr::mutate_se possible name conflicts in assigment")
      }
    }
    if(splitTerms) {
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

