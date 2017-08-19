
#' mutate standard evaluation interface.
#'
#' Mutate a data frame by the mutateTerms.  Accepts arbitrary text as
#' mutateTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#'
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_at}}, \code{\link[seplyr]{:=}}
#'
#' @param .data data.frame
#' @param mutateTerms character vector of column expressions to mutate by.
#' @return .data with altered columns.
#'
#' @examples
#'
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' resCol1 <- "Sepal_Long"
#' datasets::iris %>%
#'   mutate_se(c(resCol1 := "Sepal.Length >= 2 * Sepal.Width",
#'               "Petal_Short" := "Petal.Length <= 3.5")) %>%
#'   head()
#'
#'
#' @export
#'
mutate_se <- function(.data, mutateTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  env <- parent.frame()
  mutateQ <- lapply(mutateTerms,
                    function(si) {
                      rlang::parse_quosure(si,
                                           env = env)
                    })
  dplyr::mutate(.data = .data, !!!mutateQ)
}

