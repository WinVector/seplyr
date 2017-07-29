

#' Named map builder.
#'
#' Set names of right-argument to be left-argument, and return right argument.
#'
#' @param names names to set.
#' @param values values to assign names to (and return).
#' @return values with names set.
#'
#' @examples
#'
#'
#' c('a' := '4', 'b' := '5')
#' # equivalent to: c(a = '4', b = '5')
#'
#' c('a', 'b') := c('1', '2')
#' # equivalent to: c(a = '1', b = '2')
#'
#' # the important example
#' name <- 'a'
#' name := '5'
#' # equivalent to: c('a' = '5')
#'
#' @export
named_map_builder <- function(names, values) {
  # sepcial case 'a' := c('b', 'c') -> a := 'bc'
  if((length(values)>1)&&(length(names)==1)) {
    values <- do.call(paste0, as.list(values))
  }
  # main case
  names(values) <- as.character(names)
  values
}

#' @rdname named_map_builder
#' @export
`:=` <- function(names, values) UseMethod(":=")

# override as few S3 types as we reasonably need.
# deliberaterly leave default alone
# as a "good citizen".

# #' @export
# `:=.default` <- named_map_builder

#' @export
`:=.character` <- named_map_builder

#' @export
`:=.list` <- named_map_builder

#' @export
`:=.name` <- named_map_builder


