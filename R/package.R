#' \code{seplyr}: Standard Evaluation Interfaces for Common \code{dplyr} Verbs
#'
#' Supplies replacement interfaces for dplyr that take
#' variable names as simple strings.  Also supplies some useful "packaged pipelines"
#' including \code{group_summarize} (also eliminates need for \code{dplyr::add_count} and \code{dplyr::add_tally}), \code{add_group_summaries}, and \code{add_group_indices}.
#'
#'
#' @docType package
#' @name seplyr
NULL

#' @importFrom dplyr arrange desc distinct group_by group_indices left_join mutate one_of rename select summarize ungroup
#' @importFrom rlang parse_expr sym syms
NULL
