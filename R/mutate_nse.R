



#' mutate non-standard evaluation interface.
#'
#' Mutate a data frame by the mutate terms from \code{...}.
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
#' @examples
#'
#'
#' resCol1 <- "Sepal_Long"
#' limit <- 3.5
#'
#' datasets::iris %.>%
#'   mutate_nse(., resCol1 := Sepal.Length >= 2 * Sepal.Width,
#'                 Petal_Short := Petal.Length <= limit) %.>%
#'   head(.)
#'
#'
#' @export
#'
mutate_nse <- function(.data, ...,
                       mutate_nse_split_terms = TRUE,
                       mutate_nse_env = parent.frame(),
                       mutate_nse_printPlan = FALSE) {
  mutateTerms <- wrapr::qae(...)
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::mutate_nse first argument must be a data.frame or tbl")
  }
  res <- .data
  if(length(mutateTerms)>0) {
    res <- mutate_se(res, mutateTerms,
                     splitTerms = mutate_nse_split_terms,
                     env = mutate_nse_env,
                     printPlan = mutate_nse_printPlan)
  }
  res
}

