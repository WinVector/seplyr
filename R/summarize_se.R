
#' summarize standard interface.
#'
#' summarize a data frame by the summarizeTerms.  Accepts arbitrary text as
#' summarizeTerms to allow forms such as "mean(Sepal.Length)".
#'
#' @seealso \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param summarizeTerms character vector of column expressions to summarize by.
#' @param env environment to work in.
#' @return .data with summarizeTerms summarization applied.
#'
#' @examples
#'
#'
#' datasets::iris %.>%
#'   summarize_se(., qae(Mean_Sepal_Length := mean(Sepal.Length),
#'                       Max_Sepal_Length := max(Sepal.Length)))
#'
#'
#' @export
#'
summarize_se <- function(.data, summarizeTerms,  env=parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::summarize_se first argument must be a data.frame or tbl")
  }
  # convert char vector into spliceable vector
  # from: https://github.com/tidyverse/rlang/issues/116
  summarizeQ <- lapply(summarizeTerms,
                    function(si) {
                      rlang::parse_quosure(si,
                                           env = env)
                    })
  dplyr::summarize(.data = .data, !!!summarizeQ)
}


#' @rdname summarize_se
#' @export
summarise_se <- summarize_se

