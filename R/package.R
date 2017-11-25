#' \code{seplyr}: Standard Evaluation Improved Interfaces for Common Data Manipulatio Tasks
#'
#' The \code{seplyr} (standard evaluation \code{dplyr}) package supplies improved
#' standard evaluation adapter methods for important common data manipulation tasks.
#'
#' In addition the \code{seplyr} package supplies several new "key operations
#' bound together" methods.  These include \code{group_summarize()} (which
#' combines grouping, arranging and calculation in an atomic unit),
#' \code{add_group_summaries()} (which joins grouped summaries into a \code{data.frame}
#' in a well documented manner), \code{add_group_indices()} (which adds
#' per-group identifiers to a \code{data.frame} without depending on row-order),
#' \code{partition_mutate_qt()} (which optimizes mutate sequences), and \code{if_else_device()}
#' (which simulates per-row if-else blocks in expression sequences).
#'
#'
#' @docType package
#' @name seplyr
NULL

#' @importFrom wrapr := let lambda %.>%
NULL

# make . not look like an undefined ref
. <- NULL

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




