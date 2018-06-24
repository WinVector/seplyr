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
#' @examples
#' # data frame used to illustrate tidyr::complete()
#' library(dplyr, warn.conflicts = FALSE)
#' library(tidyr) # for nesting()
#' df <- tibble(
#' group = c(1:2, 1),
#' item_id = c(1:2, 2),
#' item_name = c("a", "b", "b"),
#' value1 = 1:3,
#' value2 = 4:6)
#'
#' # columns to complete by
#' col_terms <- c("group", "item_id", "item_name")
#' df %.>% complete_se(., col_terms)
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0))
#'
#' # with nesting
#' col_terms <- c("group", "nesting(item_id, item_name)")
#' df %.>% complete_se(., col_terms)
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0))
#' df %.>% complete_se(., col_terms, fill = list(value1 = 0, value2 = 0))
#'
#' @export
complete_se <- function(data, col_terms, fill = list(), env = parent.frame()) {
	if(!(is.data.frame(data) || dplyr::is.tbl(data))) {
		stop("seplyr::complete_se first argument must be a data.frame or tbl")
	}
	# convert char vector into spliceable vector
	# from: https://github.com/tidyverse/rlang/issues/116
	col_syms <- lapply(
		col_terms,
		function(si) {
			rlang::parse_quo(si, env = env)
		})
	tidyr::complete(data, !!!col_syms, fill = fill)
}

