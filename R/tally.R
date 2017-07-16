
#' tally/count standard interface.
#'
#' Add a new column named "n" with (optionally per-group) sums/counts.
#'
#' Note: \code{dplyr::count}, \code{dplyr::add_count},
#' \code{dplyr::tally}, and \code{dplyr::add_tally} are not \code{S3}
#' methods, so it may not be practical to re-dispatch \code{seplyr} calls
#' to these \code{dplyr} implementations.
#'
#' @seealso \code{\link[dplyr]{tally}}
#'
#' @param x data.frame to tally/count
#' @param wt character optional column name containing row-weights (passed to count/tally)
#' @param sort logical if TRUE sort result in descending order
#' @return .data with only selected columns
#'
#' @examples
#'
#' datasets::mtcars %>% tally_se()
#' datasets::mtcars %>% tally_se(wt = "cyl")
#'
#' @export
#'
tally_se <- function(x, wt=NULL, sort = FALSE) {
  if(is.null(wt)) {
    tally(x, sort=sort)
  } else {
    tally(x, wt = !!rlang::sym(wt), sort=sort)
  }
}

#' tally/count standard interface.
#'
#' Add a new column named "n" with (optionally per-group) sums/counts.
#'
#' Note: \code{dplyr::count}, \code{dplyr::add_count},
#' \code{dplyr::tally}, and \code{dplyr::add_tally} are not \code{S3}
#' methods, so it may not be practical to re-dispatch \code{seplyr} calls
#' to these \code{dplyr} implementations.
#'
#' @seealso \code{\link[dplyr]{add_tally}}
#'
#' @param x data.frame to tally/count
#' @param wt character optional column name containing row-weights (passed to count/tally)
#' @param sort logical if TRUE sort result in descending order
#' @return .data with only selected columns
#'
#' @examples
#'
#' datasets::iris %>% add_tally_se()
#'
#' @export
#'
add_tally_se <- function(x, wt=NULL, sort = FALSE) {
  if(is.null(wt)) {
    add_tally(x, sort=sort)
  } else {
    add_tally(x, wt = !!rlang::sym(wt), sort=sort)
  }
}



#' tally/count standard interface.
#'
#' Add a new column named "n" with (optionally per-group) sums/counts.
#'
#' Note: \code{dplyr::count}, \code{dplyr::add_count},
#' \code{dplyr::tally}, and \code{dplyr::add_tally} are not \code{S3}
#' methods, so it may not be practical to re-dispatch \code{seplyr} calls
#' to these \code{dplyr} implementations.
#'
#' @seealso \code{\link[dplyr]{count}}
#'
#' @param x data.frame to tally/count
#' @param groupingVars character vector of column names to group by.
#' @param wt character optional column name containing row-weights (passed to count/tally)
#' @param sort logical if TRUE sort result in descending order
#' @return .data with only selected columns
#'
#' @examples
#'
#' datasets::mtcars %>% count_se(groupingVars= c('cyl', 'gear'))
#'
#' @export
#'
count_se <- function(x, groupingVars = NULL,
                     wt=NULL, sort = FALSE) {
  groupingSyms <- rlang::syms(groupingVars)
  if(is.null(wt)) {
    count(x, !!!groupingSyms, sort=sort)
  } else {
    count(x, !!!groupingSyms, wt = !!rlang::sym(wt), sort=sort)
  }
}

#' tally/count standard interface.
#'
#' Add a new column named "n" with (optionally per-group) sums/counts.
#'
#' Note: \code{dplyr::count}, \code{dplyr::add_count},
#' \code{dplyr::tally}, and \code{dplyr::add_tally} are not \code{S3}
#' methods, so it may not be practical to re-dispatch \code{seplyr} calls
#' to these \code{dplyr} implementations.
#'
#' @seealso \code{\link[dplyr]{add_count}}
#'
#' @param x data.frame to tally/count
#' @param groupingVars character vector of column names to group by.
#' @param wt character optional column name containing row-weights (passed to count/tally)
#' @param sort logical if TRUE sort result in descending order
#' @return .data with only selected columns
#'
#' @examples
#'
#' datasets::iris %>% count_se(wt = "Sepal.Width", groupingVars= c('Species'))
#'
#' @export
#'
add_count_se <- function(x, groupingVars = NULL,
                         wt=NULL, sort = FALSE) {
  groupingSyms <- rlang::syms(groupingVars)
  if(is.null(wt)) {
    add_count(x, !!!groupingSyms, sort=sort)
  } else {
    add_count(x, !!!groupingSyms, wt = !!rlang::sym(wt), sort=sort)
  }
}


