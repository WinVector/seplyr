
#' transmute non-standard evaluation interface.
#'
#' transmute a data frame by the transmuteterms from \code{...}.
#'
#' @seealso \code{\link{transmute_se}}, \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{transmute_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to transmute by.
#' @param transmute_nse_env environment to work in.
#' @param transmute_nse_warn logical, if TRUE warn about possible name collisions.
#' @return .data with altered columns(other columns dropped).
#'
#' @examples
#'
#' datasets::iris %.>%
#'   transmute_nse(., Sepal_Long := Sepal.Length >= 2 * Sepal.Width,
#'                    Petal_Short := Petal.Length <= 3.5) %.>%
#'   summary(.)
#'
#' @export
#'
transmute_nse <- function(.data, ...,
                          transmute_nse_env = parent.frame(),
                          transmute_nse_warn = TRUE) {
  transmuteTerms <- wrapr::qae(...)
  force(transmute_nse_env)
  res <- .data
  len <- length(transmuteTerms)
  if(len>1) {
    res <- transmute_se(res, transmuteTerms,
                        env = transmute_nse_env,
                        warn = transmute_nse_warn)
  }
  res
}

