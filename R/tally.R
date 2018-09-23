
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
#' @return .data with added column n, containing counts.
#'
#' @examples
#'
#'
#' datasets::mtcars %.>% tally_se(.)
#'
#'
#' datasets::mtcars %.>% tally_se(., wt = "cyl")
#'
#' @export
#'
tally_se <- function(x, wt=NULL, sort = FALSE) {
  if(!(is.data.frame(x) || dplyr::is.tbl(x))) {
    stop("seplyr::tally_se first argument must be a data.frame or tbl")
  }
  if(length(wt)>0) {
    if((!is.character(wt)) || (length(wt)!=1)) {
      stop("seplyr::tally_se wt must be length 0, or length 1 character vector")
    }
  }
  if(length(wt)<=0) {
    dplyr::tally(x, sort=sort)
  } else {
    dplyr::tally(x, wt = !!rlang::sym(wt), sort=sort)
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
#' @return .data with added column n, containing counts.
#'
#' @examples
#'
#'
#' datasets::iris %.>% add_tally_se(.)
#'
#' @export
#'
add_tally_se <- function(x, wt=NULL, sort = FALSE) {
  if(!(is.data.frame(x) || dplyr::is.tbl(x))) {
    stop("seplyr::add_tally_se first argument must be a data.frame or tbl")
  }
  if(length(wt)>0) {
    if((!is.character(wt)) || (length(wt)!=1)) {
      stop("seplyr::add_tally_se wt must be length 0, or length 1 character vector")
    }
  }
  if(length(wt)<=0) {
    dplyr::add_tally(x, sort=sort)
  } else {
    dplyr::add_tally(x, wt = !!rlang::sym(wt), sort=sort)
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
#' @return .data with added column n, containing counts.
#'
#' @examples
#'
#'
#' datasets::mtcars %.>% count_se(., groupingVars= c('cyl', 'gear'))
#'
#' @export
#'
count_se <- function(x, groupingVars = NULL,
                     wt=NULL, sort = FALSE) {
  if(!(is.data.frame(x) || dplyr::is.tbl(x))) {
    stop("seplyr::count_se first argument must be a data.frame or tbl")
  }
  if(length(groupingVars)>0) {
    if(!is.character(groupingVars)) {
      stop("seplyr::count_se groupingVars must be length 0, or a character vector")
    }
  }
  if(length(wt)>0) {
    if((!is.character(wt)) || (length(wt)!=1)) {
      stop("seplyr::count_se wt must be length 0, or length 1 character vector")
    }
  }
  if(length(groupingVars)>0) {
    groupingSyms <- rlang::syms(groupingVars)
    if(length(wt)<=0) {
      dplyr::count(x, !!!groupingSyms, sort=sort)
    } else {
      dplyr::count(x, !!!groupingSyms, wt = !!rlang::sym(wt), sort=sort)
    }
  } else {
    if(length(wt)<=0) {
      dplyr::count(x, sort=sort)
    } else {
      dplyr::count(x, wt = !!rlang::sym(wt), sort=sort)
    }
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
#' @return .data with added column n, containing counts.
#'
#' @examples
#'
#'
#' datasets::iris %.>% count_se(., wt = "Sepal.Width", groupingVars= c('Species'))
#'
#' @export
#'
add_count_se <- function(x, groupingVars = NULL,
                         wt=NULL, sort = FALSE) {
  if(!(is.data.frame(x) || dplyr::is.tbl(x))) {
    stop("seplyr::add_count_se first argument must be a data.frame or tbl")
  }
  if(length(groupingVars)>0) {
    if(!is.character(groupingVars)) {
      stop("seplyr::add_count_se groupingVars must be length 0, or a character vector")
    }
  }
  if(length(wt)>0) {
    if((!is.character(wt)) || (length(wt)!=1)) {
      stop("seplyr::add_count_se wt must be length 0, or length 1 character vector")
    }
  }
  if(length(groupingVars)>0) {
    groupingSyms <- rlang::syms(groupingVars)
    if(length(wt)<=0) {
      dplyr::add_count(x, !!!groupingSyms, sort=sort)
    } else {
      dplyr::add_count(x, !!!groupingSyms, wt = !!rlang::sym(wt), sort=sort)
    }
  } else {
    if(length(wt)<=0) {
      dplyr::add_count(x, sort=sort)
    } else {
      dplyr::add_count(x, wt = !!rlang::sym(wt), sort=sort)
    }
  }
}


