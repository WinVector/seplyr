
#' rename standard interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename_at}}).
#'
#' @seealso \code{\link{rename_mp}}, \code{\link[dplyr]{rename}},  \code{\link[dplyr]{rename_at}}, \code{\link[seplyr]{:=}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (new names on the left, original names on the right; this may seem reversed but it matches dplyr::rename()).
#' @return .data with renamed columns
#'
#' @examples
#'
#' datasets::mtcars %>%
#'    rename_se(c("cylinders" := "cyl", "gears" := "gear")) %>%
#'    head()
#' # # sames as:
#' # datasets::mtcars %>%
#' #    rename(cylinders = cyl, gears = gear) %>%
#' #    head()
#'
#' @export
#'
rename_se <- function(.data, mapping) {
  mp <- lapply(mapping, rlang::sym)
  do.call(rename, c(list(.data) , mp))
}


#' rename map interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename}}).
#'
#' @seealso \code{\link{rename_se}}, \code{\link[dplyr]{rename}},  \code{\link[dplyr]{rename_at}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (original names on the left, new names on the right; this reverse of dplyr::rename()).
#' @return .data with renamed columns
#'
#' @examples
#'
#' mapping <- c("cyl" = "cylinders", "gear" = "gears")
#' datasets::mtcars %>%
#'    rename_mp(mapping) %>%
#'    head()
#' # # sames as:
#' # datasets::mtcars %>%
#' #    rename(cylinders = cyl, gears = gear) %>%
#' #    head()
#'
#' @export
#'
rename_mp <- function(.data, mapping) {
  mp <- lapply(mapping, rlang::sym)
  mapfn <- function(ci) { mp[ci] }
  rename_at(.tbl = .data, .vars = names(mp), mapfn)
}
