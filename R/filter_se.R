
#' filter standard interface.
#'
#' Filter a data frame by the filterTerms.  Accepts arbitrary text as
#' filterTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#'
#' @seealso \code{\link[dplyr]{filter}}, \code{\link[dplyr]{filter_at}}
#'
#' @param .data data.frame
#' @param filterTerms character vector of column expressions to filter by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' upperBound <- 3.5
#' datasets::iris %>%
#'   filter_se(c("Sepal.Length >= 2 * Sepal.Width",
#'               "Petal.Length <= upperBound"))
#'
#'
#' @export
#'
filter_se <- function(.data, filterTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  filterQ <- lapply(filterTerms,
                    function(si) { rlang::parse_expr(si) })
  filter(.data = .data, !!!filterQ)
}
