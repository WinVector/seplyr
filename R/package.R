#' \code{seplyr}: Standard Evaluation Interfaces for Common \code{dplyr} Verbs
#'
#'
#' The \code{seplyr} (standard evaluation data.frame \code{dplyr}) package supplies
#' standard evaluation adapter methods for important common \code{dplyr} methods
#' that currently have a non-standard programming interface.
#'
#' This package allows
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

#' @importFrom wrapr := let lambda
NULL

#' @importFrom dplyr all_equal bind_rows bind_cols
#' @importFrom dplyr inner_join left_join right_join full_join semi_join anti_join
#' @importFrom dplyr ungroup distinct
#' @importFrom dplyr compute collect tbl is.tbl as.tbl copy_to

#' @export
dplyr::all_equal

#' @export
dplyr::bind_rows

#' @export
dplyr::bind_cols

#' @export
dplyr::inner_join

#' @export
dplyr::left_join

#' @export
dplyr::right_join

#' @export
dplyr::full_join

#' @export
dplyr::semi_join

#' @export
dplyr::anti_join

#' @export
dplyr::ungroup

#' @export
dplyr::distinct

#' @export
dplyr::compute

#' @export
dplyr::collect

#' @export
dplyr::tbl

#' @export
dplyr::is.tbl

#' @export
dplyr::as.tbl

#' @export
dplyr::copy_to




