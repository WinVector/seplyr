
#' mutate standard evaluation interface.
#'
#' Mutate a data frame by the mutateTerms.  Accepts arbitrary text as
#' mutateTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#' Terms are vectors or lists of the form "lhs := rhs".
#' Semantics are: terms are evaluated left to right if splitTerms==TRUE (the default).
#'
#' @seealso \code{\link{mutate_nse}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{mutate_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param mutateTerms character vector of column expressions to mutate by.
#' @param splitTerms logical, if TRUE into separate mutates (if FALSE instead, pass all at once to dplyr).
#' @param env environment to work in.
#' @return .data with altered columns.
#'
#' @examples
#'
#'
#' resCol1 <- "Sepal_Long"
#' limit <- 3.5
#'
#' datasets::iris %.>%
#'   mutate_se(., c(resCol1 := "Sepal.Length >= 2 * Sepal.Width",
#'                  "Petal_Short" := "Petal.Length <= limit")) %.>%
#'   head(.)
#'
#'
#' @export
#'
mutate_se <- function(.data, mutateTerms,
                      splitTerms = TRUE,
                      env = parent.frame()) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  res <- .data
  if(length(mutateTerms)>0) {
    mutateQ <- lapply(mutateTerms,
                      function(si) {
                        rlang::parse_quosure(si,
                                             env = env)
                      })
    if(splitTerms) {
      for(ti in names(mutateQ)) {
        si <- rlang::sym(ti)
        vi <- mutateQ[[ti]]
        res <- dplyr::mutate(.data = res, !!si := !!!vi)
      }
    } else {
      res <- dplyr::mutate(.data = res, !!!mutateQ)
    }
  }
  res
}

