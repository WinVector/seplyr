

check_is_char_vec_or_listscal <- function(v) {
  if(length(v)<=0) {
    return(TRUE)
  }
  if(is.character(v)) {
    return(TRUE)
  }
  if(!is.list(v)) {
    return(FALSE)
  }
  is_char <- vapply(v, is.character, logical(1))
  if(!all(is_char)) {
    return(FALSE)
  }
  lens <- vapply(v, length, numeric(1))
  if(!all(lens==1)) {
    return(FALSE)
  }
  TRUE
}

#' filter standard interface.
#'
#' Filter a data frame by the filterTerms.  Accepts arbitrary text as
#' filterTerms to allow forms such as "Sepal.Length >= 2 * Sepal.Width".
#'
#' @seealso \code{\link[dplyr]{filter}}, \code{\link[dplyr]{filter_at}}
#'
#' @param .data data.frame
#' @param filterTerms character vector or list of column expressions to filter by.
#' @param env environment to work in.
#' @return .data filtered by columns named in filterTerms
#'
#' @examples
#'
#' upperBound <- 3.5
#'
#' datasets::iris %.>%
#'   filter_se(., qe(Sepal.Length >= 2 * Sepal.Width,
#'                   Petal.Length <= upperBound))
#'
#'
#' @export
#'
filter_se <- function(.data, filterTerms,  env=parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::filter_se first argument must be a data.frame or tbl")
  }
  if(length(filterTerms)<=0) {
    return(.data)
  }
  if(!check_is_char_vec_or_listscal(filterTerms)) {
    stop("seplyr::filter_se filterTerms must be a character vector or list of character scalars")
  }
  force(env)
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  # updated: https://github.com/WinVector/seplyr/issues/3
  filterQ <- lapply(filterTerms,
                    function(si) {
                      rlang::parse_quo(si,
                                       env = env)
                    })
  dplyr::filter(.data = .data, !!!filterQ)
}
