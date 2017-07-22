
#' mutate standard interface.
#'
#' Mutate a data frame by the mutateTerms.  Accepts arbitrary text as
#' mutateTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form
#'
#' @seealso \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_at}}, \code{\link[seplyr]{:=}}
#'
#' @param .data data.frame
#' @param mutateTerms character vector of column expressions to mutate by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' datasets::iris %>%
#'   mutate_se(c("Sepal_Long" := "Sepal.Length >= 2 * Sepal.Width",
#'               "Petal_Short" := "Petal.Length <= 3.5")) %>%
#'   summary()
#'
#'
#' @export
#'
mutate_se <- function(.data, mutateTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  mutateQ <- lapply(mutateTerms,
                    function(si) { rlang::parse_expr(si) })
  mutate(.data = .data, !!!mutateQ)
}
