#' select standard interface.
#'
#' select columns.  To remove columns please see \code{\link{deselect}}.
#'
#' @seealso \code{\link{deselect}}, \code{\link[dplyr]{select}}, \code{\link[dplyr]{select_at}}
#'
#' @param .data data.frame
#' @param colNames character vector of columns to keep
#' @return .data with only selected columns
#'
#' @examples
#'
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' datasets::mtcars %>%
#'    select_se(c("cyl", "gear")) %>%
#'    head()
#' # essentially dplyr::select_at()
#'
#' data.frame(a=1, b=2) %>% select_se('-b')
#'
#' @export
#'
select_se <- function(.data, colNames) {
  # select(.data, one_of(colNames))
  # convert char vector into spliceable vector
  # use full parse instead of sym() to allow -var forms
  # colSyms <- lapply(colNames,
  #                   function(si) { rlang::parse_expr(si) })
  # parse_expr sees into global environment
  #  https://github.com/tidyverse/dplyr/issues/3006
  env <- new.env(parent = emptyenv())
  assign('-', `-`, envir = env)
  assign('(', `(`, envir = env)
  colSyms <- lapply(colNames,
                    function(si) {
                      rlang::parse_quosure(si,
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
#' suppressPackageStartupMessages(library("dplyr"))
#'
#' datasets::mtcars %>%
#'    deselect(c("cyl", "gear")) %>%
#'    head()
#' # essentially dplyr::select( datasets::mtcars, -cyl, -gear)
#'
#' @export
#'
deselect <- function(.data, colNames) {
  # select(.data, one_of(colNames))
  # convert char vector into spliceable vector
  colSyms <- rlang::syms(setdiff(colnames(.data), colNames))
  dplyr::select(.data, !!!colSyms)
}
