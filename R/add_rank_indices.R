
#' Arrange a data frame and rank indexes.
#'
#' @param .data data.frame
#' @param ... force later arguments to bind by name.
#' @param arrangeTerms character vector of column expressions to arrange by.
#' @param orderColumn character name of column to add in-group order marks to.
#' @return .data with order indices added (no ties).
#'
#' @examples
#'
#'
#' datasets::mtcars %>%
#'   # tibble::rownames_to_column() not currently re-exported by dplyr
#'   { mutate(., CarName = rownames(.) ) } %>%
#'   select_se(c('CarName', 'hp', 'wt')) %>%
#'   add_rank_indices(arrangeTerms = c('desc(hp)', 'wt'),
#'                    orderColumn = 'rankID') %>%
#'   arrange_se(c('rankID'))
#'
#'
#' @export
#'
add_rank_indices <- function(.data,
                             ...,
                             arrangeTerms = NULL,
                             orderColumn) {
  if(length(list(...))>0) {
    stop("seplyr::add_rank_indices unexpected arguments")
  }
  `:=` <- NULL # don't let look like an unbound reference to CRAN checker
  if(is.null(arrangeTerms)) {
    arrangeTerms <- colnames(.data)
  }
  # from: https://github.com/tidyverse/rlang/issues/116
  arrangeTerms <- lapply(arrangeTerms,
                         function(si) { rlang::parse_expr(si) })
  .data <- arrange(.data, !!!arrangeTerms)
  # add ordered row-ids globally
  d <- mutate(.data, !!orderColumn := 1 )
  d <- mutate(d, !!orderColumn := cumsum(!!rlang::sym(orderColumn)) )
  d
}
