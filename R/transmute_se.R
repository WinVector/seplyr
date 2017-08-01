
#' transmute standard interface.
#'
#' transmute a data frame by the transmuteTerms.  Accepts arbitrary text as
#' transmuteTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#'
#' @seealso \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{transmute_at}}, \code{\link[seplyr]{:=}}
#'
#' @param .data data.frame
#' @param transmuteTerms character vector of column expressions to transmute by.
#' @return .data grouped by columns named in groupingVars
#'
#' @examples
#'
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' datasets::iris %>%
#'   transmute_se(c("Sepal_Long" := "Sepal.Length >= 2 * Sepal.Width",
#'               "Petal_Short" := "Petal.Length <= 3.5")) %>%
#'   summary()
#'
#'
#' @export
#'
transmute_se <- function(.data, transmuteTerms) {
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  env <- parent.frame()
  transmuteQ <- lapply(transmuteTerms,
                       function(si) {
                         rlang::parse_quosure(si,
                                              env = env)
                       })
  dplyr::transmute(.data = .data, !!!transmuteQ)
}
