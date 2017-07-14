#' \code{seplyr}: Standard Evaluation Interfaces for Common \code{dplyr} Verbs
#'
#' The \code{seplyr} (standard evaluation data.frame \code{dplyr}) package supplies
#' standard evaluation adapter methods for important common \code{dplyr} methods
#' that currently have a non-standard programming interface. This allows
#' the analyst to use \code{dplyr} to perform fundamental data transformation
#' steps such as arranging rows, grouping rows, aggregating selecting
#' columns without having to use learn the details of \code{rlang}/\code{tidyeval}
#' non-standard evaluation and without continuing to rely on now
#' deprecated \code{dplyr} "underscore verbs."
#'
#' In addition the \code{seplyr} package supplies several new "key operations
#' bound together" methods.  These include \code{group_summarize()} (which
#' combines grouping, arranging and calculation in an atomic unit),
#' \code{add_group_summaries()} (which joins grouped summaries into a \code{data.frame}
#' in a well documented manner), and \code{add_group_indices()} (which adds
#' per-group identifies to a \code{data.frame} without depending on row-order).
#'
#'
#' @docType package
#' @name seplyr
NULL

#' @importFrom dplyr arrange desc distinct group_by group_indices left_join mutate one_of rename select summarize ungroup
#' @importFrom rlang parse_expr sym syms
NULL
