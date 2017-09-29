
#' rename standard interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename_at}}).
#'
#' @seealso \code{\link[dplyr]{rename}},  \code{\link[dplyr]{rename_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (new names on the left, original names on the right; this may seem reversed but it matches dplyr::rename()).
#' @return .data with renamed columns
#'
#' @examples
#'
#'
#' datasets::mtcars %.>%
#'    rename_se(., c("cylinders" := "cyl", "gears" := "gear")) %.>%
#'    head(.)
#' # # sames as:
#' # datasets::mtcars %>%
#' #    rename(cylinders = cyl, gears = gear) %>%
#' #    head()
#'
#' @export
#'
rename_se <- function(.data, mapping) {
  mp <- lapply(mapping, rlang::sym)
  do.call(dplyr::rename, c(list(.data) , mp))
}

