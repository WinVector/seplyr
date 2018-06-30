
#' Collect values from blocks of rows into columns.
#'
#' Standared inteface to \code{\link[tidyr]{spread}}.
#' Take values from the columns named in the \code{columns} argument and
#' move them into blocks of rows, placing values in the new column specified by \code{value}
#' and indicating which column each value came from in the new column specified by \code{key}.
#'
#' @param data data.frame to take values from.
#' @param key character, name for existing column to get new column names from.
#' @param value character, name for existing column to take values from.
#' @param ... not used, force later arguments to bind by name.
#' @param fill passed to spread.
#' @param convert passed to spread.
#' @param drop passed to spread.
#' @param sep passed to spread.
#' @return converted data.
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'     'id', 'name_for_new_column' , 'value_to_take' |
#'     1   , 'col1'                , 'a'             |
#'     1   , 'col2'                , '10'            |
#'     2   , 'col1'                , 'b'             |
#'     2   , 'col2'                , '20'            )
#' spread_se(d,
#'    key = 'name_for_new_column',
#'    value = 'value_to_take')
#'
#' @seealso \code{\link[tidyr]{spread}}, \code{\link{gather_se}}
#'
#' @export
#'
spread_se <- function(data,
                      key, value,
                      ...,
                      fill = NA,
                      convert = FALSE,
                      drop = TRUE,
                      sep = NULL) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "seplyr::gather_se")
  if(!(is.data.frame(data) || dplyr::is.tbl(data))) {
    stop("seplyr::spread_se first argument must be a data.frame or tbl")
  }
  if((!is.character(key))||(length(key)!=1)) {
    stop("seplyr::spread_se key must be a single string")
  }
  if((!is.character(value))||(length(value)!=1)) {
    stop("seplyr::spread_se value must be a single string")
  }
  tidyr::spread(data,
                key = !!key,
                value = !!value,
                fill = fill,
                convert = convert,
                drop = drop,
                sep = sep)
}
