
#' Select columns standard interface.
#'
#' Select columns.  To remove columns please see \code{\link{deselect}}.  Also accepts -column notation.
#'
#' @seealso \code{\link{deselect}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{select_at}}
#'
#' @param .data data.frame
#' @param colNames character vector of columns to keep
#' @return .data with only selected columns
#'
#' @examples
#'
#'
#' datasets::mtcars %.>%
#'    select_se(., c("cyl", "gear")) %.>%
#'    head(.)
#' # essentially dplyr::select_at()
#'
#' data.frame(a=1, b=2) %.>% select_se(., '-b')
#'
#' @export
#'
select_se <- function(.data, colNames) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::select_se first argument must be a data.frame or tbl")
  }
  # select(.data, one_of(colNames))
  # convert char vector into spliceable vector
  # use full parse instead of sym() to allow -var forms
  # colSyms <- lapply(colNames,
  #                   function(si) { rlang::parse_expr(si) })
  # parse_expr sees into global environment
  #  https://github.com/tidyverse/dplyr/issues/3006
  # updated: https://github.com/WinVector/seplyr/issues/3
  if(!is.character(colNames)) {
    stop("seplyr::select_se colNames must be a character vector")
  }
  env <- new.env(parent = emptyenv())
  penv <- parent.frame()
  for(sym in c("-", "c", ":", "(")) {
    assign(sym, get(sym, envir = penv), envir = env)
  }
  colSyms <- lapply(colNames,
                    function(si) {
                      rlang::parse_quo(si,
                                       env = env)
                    })
  dplyr::select(.data, !!!colSyms)
}



#' deselect standard interface.
#'
#' deselect columns.  To keep columns please see \code{\link{select_se}}.
#'
#' @seealso \code{\link{select_se}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{select_at}}
#'
#' @param .data data.frame
#' @param colNames character vector of columns to remove
#' @return .data without deselected columns
#'
#' @examples
#'
#'
#' datasets::mtcars %.>%
#'    deselect(., c("cyl", "gear")) %.>%
#'    head(.)
#' # essentially dplyr::select( datasets::mtcars, -cyl, -gear)
#'
#' @export
#'
deselect <- function(.data, colNames) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::deselect first argument must be a data.frame or tbl")
  }
  if(!is.character(colNames)) {
    stop("seplyr::deselect colNames must be a character vector")
  }
  # select(.data, one_of(colNames))
  # convert char vector into spliceable vector
  colSyms <- rlang::syms(setdiff(colnames(.data), colNames))
  dplyr::select(.data, !!!colSyms)
}
