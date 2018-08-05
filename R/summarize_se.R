
#' summarize standard interface.
#'
#' summarize a data frame by the summarizeTerms.  Accepts arbitrary text as
#' summarizeTerms to allow forms such as "mean(Sepal.Length)".
#'
#' @seealso \code{\link[dplyr]{summarize}}, \code{\link[dplyr]{summarize_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param summarizeTerms character vector of column expressions to summarize by.
#' @param ... force later terms to be bound by name
#' @param summarize_se logical, if TRUE warn about possible name collisions.
#' @param env environment to work in.
#' @return .data with summarizeTerms summarization applied.
#'
#' @examples
#'
#' # good
#' datasets::iris %.>%
#'   summarize_se(., qae(Mean_Sepal_Length := mean(Sepal.Length),
#'                       Max_Sepal_Length := max(Sepal.Length)))
#'
#' # good
#' datasets::iris %.>%
#'   summarize_se(., qae(Sepal.Length := mean(Sepal.Length)))
#'
#' # generates a warning
#' datasets::iris %.>%
#'   summarize_se(., qae(Sepal.Length := mean(Sepal.Length),
#'                       Max_Sepal_Length := max(Sepal.Length)))
#'
#'
#' @export
#'
summarize_se <- function(.data, summarizeTerms,
                         ...,
                         summarize_se = TRUE,
                         env = parent.frame()) {
  wrapr::stop_if_dot_args(substitute(list(...)), "seplyr::summarize_se")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::summarize_se first argument must be a data.frame or tbl")
  }
  if(summarize_se) {
    plan <- partition_mutate_se(summarizeTerms)
    if(length(plan)!=1) {
      warning(paste("seplyr::summarize_se possibly confusing column name re-use",
                    wrapr::map_to_char(summarizeTerms)))
    }
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

