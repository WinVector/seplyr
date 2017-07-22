
#' summarize standard interface.
#'
#' summarize a data frame by the summarizeTerms.  Accepts arbitrary text as
#' summarizeTerms to allow forms such as "mean(Sepal.Length)".
#'
#' @seealso \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[seplyr]{:=}}
#'
#' @param .data data.frame
#' @param summarizeTerms character vector of column expressions to summarize by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' datasets::iris %>%
#'   summarize_se(c("Mean_Sepal_Length" := "mean(Sepal.Length)",
#'                  "Max_Sepal_Length" := "max(Sepal.Length)")) %>%
#'   head()
#'
#'
#' @export
#'
summarize_se <- function(.data, summarizeTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  summarizeQ <- lapply(summarizeTerms,
                    function(si) { rlang::parse_expr(si) })
  summarize(.data = .data, !!!summarizeQ)
}


#' @rdname summarize_se
#' @export
summarise_se <- summarize_se

