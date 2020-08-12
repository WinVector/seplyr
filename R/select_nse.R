#' Select columns non-standard (code capturing) interface.
#'
#' Select column that are exactly the names captured unevaluated from \code{...}.
#' This is to provide a simple interface that reliably uses non-standard captured names
#' (and not consequences of further evaluation).  Please see
#' \url{https://win-vector.com/2018/09/23/a-subtle-flaw-in-some-popular-r-nse-interfaces/}
#' for some discussion.  Also accepts -name notation, but not integers or
#' functions of columns.  Does not look at argument names (so can not be used to rename columns).
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
#' # deletes wrong column!
#' dplyr::select(data.frame(x = 1, z = 3), -y)
#'
#' # throws when y is not the name of a column (good)
#' tryCatch(
#'   select_nse(data.frame(x = 1), y),
#'   error = function(e) { e }
#' )
#'
#' #' # throws when y is not the name of a column (good)
#' tryCatch(
#'   select_nse(data.frame(x = 1, z = 3), -y),
#'   error = function(e) { e }
#' )
#'
#' @export
#'
select_nse <- function(.data, ...) {
  colSyms <- eval(substitute(alist(...)))
  cmap <- as.list(colnames(.data))
  names(cmap) <- cmap
  colNames <- vapply(
    colSyms,
    function(ci) {
      if(is.numeric(ci)) {
        stop("seplyr::select_nse does not accept numeric arguments")
      }
      if(is.name(ci) || is.character(ci)) {
        return(as.character(ci))
      }
      if(is.call(ci)) {
        if(as.character(ci[[1]])=='-') {
          return(deparse(ci))
        }
        stop("seplyr::select_nse does not accept calls other than '-'")
      }
      stop(paste("seplyr::select_nse does not accept class",
                 paste(class(ci), collapse = " ")))
    }, character(1))
  select_se(.data, colNames)
}
