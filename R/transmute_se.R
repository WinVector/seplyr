
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
#' @param warn logical, if TRUE warn about possible name collisions.
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
transmute_se <- function(.data, transmuteTerms,
                         env = parent.frame(),
                         warn = TRUE) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::transmute_se first argument must be a data.frame or tbl")
  }
  if(!check_is_char_vec_or_listscal(transmuteTerms)) {
    stop("seplyr::transmute_se transmuteTerms must be a character vector or list of character scalars")
  }
  force(env)
  if(warn) {
    plan <- partition_mutate_se(transmuteTerms)
    if(length(plan)!=1) {
      warning(paste("seplyr::transmute_se possibly confusing column name re-use",
                    wrapr::map_to_char(transmuteTerms)))
    }
  }
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  # updated: https://github.com/WinVector/seplyr/issues/3
  transmuteQ <- lapply(transmuteTerms,
                       function(si) {
                         rlang::parse_quo(si,
                                          env = env)
                       })
  dplyr::transmute(.data = .data, !!!transmuteQ)
}
