#' @importFrom tidyr complete nesting
#' @importFrom rlang parse_quo
#' @importFrom dplyr tibble
NULL

#' complete by standard interface
#'
#' Complete a data frame with missing combinations of data. Turns implicit missing values into explicit missing values.
#'
#' This is a standard evaluation interface for \code{tidyr::complete()}. The purpose of the function is to be able to use a vector of characters (column names) as the argument for expanding the data frame.
#'
#' @param data A data frame or tbl.
#' @param col_terms A character vector of column names or expressions to complete by.
#' @param fill A list that for each variable supplies a single value to use instead of NA for missing combinations.
#' @param env The environment as an argument (in case the function is called from another function).
#' @return The data frame with implicit missing values identified.
#'
#' @examples
#'
#' # data frame used to illustrate tidyr::complete()
#' df <- wrapr::build_frame(
#'   "group"  , "item_id", "item_name", "value1", "value2" |
#'     1      , 1        , "a"        , 1L      , 4L       |
#'     2      , 2        , "b"        , 2L      , 5L       |
#'     1      , 2        , "b"        , 3L      , 6L       )
#'
#' # columns to complete by
#' col_terms <- qc(group, item_id, item_name)
#' df %.>% complete_se(., col_terms)
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0))
#'
#' # with nesting
#' col_terms <- c("group", "tidyr::nesting(item_id, item_name)")
#' df %.>% complete_se(., col_terms)
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0))
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0, value2 = 0))
#'
#' @export
complete_se <- function(data, col_terms, fill = list(), env = parent.frame()) {
	if(!(is.data.frame(data) || dplyr::is.tbl(data))) {
		stop("seplyr::complete_se first argument must be a data.frame or tbl")
	}
  if(!is.character(col_terms)) {
    stop("seplyr::complete_se col_terms must be a character vector")
  }
  force(env)
	# convert char vector into spliceable vector
	# from: https://github.com/tidyverse/rlang/issues/116
	col_syms <- lapply(
		col_terms,
		function(si) {
			rlang::parse_quo(si, env = env)
		})
	tidyr::complete(data, !!!col_syms, fill = fill)
}

