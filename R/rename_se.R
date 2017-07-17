
#' rename standard interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename_at}}).
#'
#' @seealso \code{\link[dplyr]{rename}},  \code{\link[dplyr]{rename_at}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (new news on the left, original names on the right; this may seem reversed but it matches dplyr::rename()).
#' @return .data with renamed columns
#'
#' @examples
#'
#' datasets::mtcars %>%
#'    rename_se(c("cylinders" = "cyl", "gears" = "gear")) %>%
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
  return(do.call(rename, c(list(.data) , mp)))
  # # alternate impl
  # mp <- rlang::syms(names(mapping))
  # names(mp) <- as.character(mapping)
  # mapfn <- function(ci) { mp[ci] }
  # return(rename_at(.tbl = .data, .vars = names(mp), mapfn))
}
