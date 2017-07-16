
#' rename standard interface.
#'
#' rename columns.
#'
#' @seealso \code{\link[dplyr]{rename}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (new news on the left, original names on the right).
#' @return .data with renamed columns
#'
#' @examples
#'
#' datasets::mtcars %>% rename_se(c("cylinders" = "cyl", "gears" = "gear")) %>% head()
#'
#' @export
#'
rename_se <- function(.data, mapping) {
  mp <- lapply(mapping, rlang::sym)
  do.call(rename, c(list(.data) , mp))
}
