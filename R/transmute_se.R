
#' transmute standard interface.
#'
#' transmute a data frame by the transmuteTerms.  Accepts arbitrary text as
#' transmuteTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#'
#' @seealso \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{transmute_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param transmuteTerms character vector of column expressions to transmute by.
#' @param env environment to work in.
#' @return .data transumuted by transmuteTerms.
#'
#' @examples
#'
#'
#' datasets::iris %.>%
#'   transmute_se(., qae(Sepal_Long := Sepal.Length >= 2 * Sepal.Width,
#'                       Petal_Short := Petal.Length <= 3.5)) %.>%
#'   summary(.)
#'
#'
#' @export
#'
transmute_se <- function(.data, transmuteTerms,  env=parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::transmute_se first argument must be a data.frame or tbl")
  }
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  transmuteQ <- lapply(transmuteTerms,
                       function(si) {
                         rlang::parse_quosure(si,
                                              env = env)
                       })
  dplyr::transmute(.data = .data, !!!transmuteQ)
}
