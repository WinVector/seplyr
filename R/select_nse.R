#' Select columns non-standard (code capturing) interface.
#'
#' Select column that are exactly the names captured unevaluated from \code{...}.
#' This is to provide a simple interface that reliably uses non-standard captured names
#' (and not consequences of further evaluation).  Please see
#' \url{http://www.win-vector.com/blog/2018/09/a-subtle-flaw-in-some-popular-r-nse-interfaces/}
#' for some discussion.  Also accepts -name notation.
#'
#' @param .data data frame or tbl to select columns from.
#' @param ... unevaluated symbols to use as column names.
#'
#' @examples
#'
#' y <- "x"
#'
#' # returns y-column
#' dplyr::select(data.frame(x = 1, y = 2), y)
#'
#' # returns x-column (very confusing!)
#' dplyr::select(data.frame(x = 1), y)
#'
#' # returns y-column
#' select_nse(data.frame(x = 1, y = 2), y)
#'
#' # throws when y is not the name of a column (good)
#' tryCatch(
#'   select_nse(data.frame(x = 1), y),
#'   error = function(e) { e }
#' )
#'
#' @export
#'
select_nse <- function(.data, ...) {
  colSyms <- eval(substitute(alist(...)))
  colNames <- vapply(
    colSyms,
    function(ci) {
      as.character(ci) # deparse?
    }, character(1))
  select_se(.data, colNames)
}
