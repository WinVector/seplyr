#' Named map builder.
#'
#' Set names of right-argument to be left-argument, and return right argument.
#'
#' @param names names to set.
#' @param values values to assign names to (and return).
#' @return values with names set.
#'
#'
#' @aliases named_map_builder
#'
#'
#' @examples
#'
#'
#' c('a' := '4', 'b' := '5')
#' # equivalent to: c(a = '4', b = '5')
#'
#' c('a', 'b') := c(1, 2)
#' # equivalent to: c(a = 1, b = 2)
#'
#' # the important example
#' name <- 'a'
#' name := 5
#' # equivalent to: c("a" = 5)
#'
#' @export
`:=` <- function(names, values) {
  names(values) <- names
  values
}

