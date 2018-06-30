
#' Distribute columns into blocks of rows.
#'
#' A standard (value-oriented) interface for \code{\link[tidyr]{gather}}.
#' Take values from the columns named in the \code{columns} argument and
#' move them into blocks of rows, placing values in the new column specified by \code{value}
#' and indicating which column each value came from in the new column specified by \code{key}.
#'
#' @param data data.frame to take values from.
#' @param ... not used, force later arguments to bind by name.
#' @param key character, name for new column to record which columns values were taken from.
#' @param value character, name for new column to record values.
#' @param columns character, names of columns to take values from.
#' @param na.rm passed to gather.
#' @param convert passed to gather.
#' @param factor_key passed to gather.
#' @param use_one_of logical, if TRUE use dplyr::one_of() instead of rlang:::`!!!`.
#' @return converted data.
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'     'id', 'measurement1', 'measurement2' |
#'     1   , 'a'           , 10             |
#'     2   , 'b'           , 20             )
#' gather_se(d,
#'   key = "value_came_from_column",
#'   value = "value_was",
#'   columns = c("measurement1", "measurement2"))
#'
#' @seealso \code{\link[tidyr]{gather}}, \code{\link{spread_se}}
#'
#' @export
#'
gather_se <- function(data,
                      ...,
                      key = "key",
                      value = "value",
                      columns = NULL,
                      na.rm = FALSE,
                      convert = FALSE,
                      factor_key = FALSE,
                      use_one_of = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "seplyr::gather_se")
  if(!(is.data.frame(data) || dplyr::is.tbl(data))) {
    stop("seplyr::gather_se first argument must be a data.frame or tbl")
  }
  if((!is.character(key))||(length(key)!=1)) {
    stop("seplyr::gather_se key must be a single string")
  }
  if((!is.character(value))||(length(value)!=1)) {
    stop("seplyr::gather_se value must be a single string")
  }
  if(!is.character(columns)) {
    stop("seplyr::gather_se columns must be a string vector")
  }
  if(use_one_of) {
    tidyr::gather(data,
                  key = !!key,
                  value = !!value,
                  dplyr::one_of(columns),
                  na.rm = na.rm,
                  convert = convert,
                  factor_key = factor_key)
  } else {
    gather_terms <- lapply(columns,
                           function(si) {
                             rlang::sym(si)
                           })
    tidyr::gather(data,
                  key = !!key,
                  value = !!value,
                  !!!gather_terms,
                  na.rm = na.rm,
                  convert = convert,
                  factor_key = factor_key)
  }
}
