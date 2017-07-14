#' select standard interface.
#'
#' select columns.
#'
#' @param .data data.frame
#' @param colNames character vector of columns to keep
#' @return .data with only selected columns
#'
#' @examples
#'
#' datasets::mtcars %>% select_se(c("cyl", "gear")) %>% head()
#'
#' @export
#'
select_se <- function(.data, colNames) {
  select(.data, one_of(colNames))
}
