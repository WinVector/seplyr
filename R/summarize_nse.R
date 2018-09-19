
#' summarize non-standard evaluation interface.
#'
#' summarize a data frame by the summarize terms from \code{...}.
#'
#' @seealso  \code{\link{summarize_se}}, \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param ... stringified expressions to summarize by.
#' @param summarize_nse_warn logical, if TRUE warn about possible name collisions.
#' @param env environment to work in.
#' @return .data with summarized columns.
#'
#' @examples
#'
#'
#' datasets::iris %.>%
#'   summarize_nse(., Mean_Sepal_Length := mean(Sepal.Length),
#'                    Max_Sepal_Length := max(Sepal.Length))
#'
#' @export
#'
summarize_nse <- function(.data,
                          ...,
                          summarize_nse_warn = TRUE,
                          env = parent.frame()) {
  summarizeTerms <- wrapr::qae(...)
  force(env)
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::summarize_nse first argument must be a data.frame or tbl")
  }
  res <- .data
  len <- length(summarizeTerms)
  if(len>1) {
    res <- summarize_se(res,
                        summarizeTerms,
                        warn = summarize_nse_warn,
                        env = env)
  }
  res
}

#' @rdname summarize_nse
#' @export
summarise_nse <- summarize_nse
