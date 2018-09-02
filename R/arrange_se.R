
#' Arrange standard interface.
#'
#' Arrange a data frame by the possibly the \code{group_vars()} (optional, but defaults to off) and \code{arrangeTerms}.  Accepts arbitrary text as
#' arrangeTerms to allow forms such as "desc(gear)". Intent is to arrange only by
#' sets of variables with desc() notations reversals, not by arbitrary expressions over variables.
#' To help enforce this parsing is performed in an empty environment (so expressions
#' such as "gear + carb" deliberately error-out).
#'
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{arrange_at}}
#'
#' @param .data data.frame
#' @param arrangeTerms character vector of column expressions to arrange by.
#' @param ... not used, force later arguments to bind by name.
#' @param .by_group logical, should data be sorted by grouping variables (if present).
#' @param strict logical if TRUE accept only name and desc(name) terms.
#' @return .data arrnaged by arrangeTerms
#'
#' @examples
#'
#' datasets::mtcars %.>%
#'   arrange_se(., c("cyl", "desc(gear)")) %.>%
#'   head(.)
#' # equivilent to dplyr/magrittr pipeline
#' # arrange(datasets::mtcars, cyl, desc(gear)) %>% head()
#'
#' # Note: arranging in the presence of groups is subtle.
#' # As grouping is an annotation, not an ordering (and ordering is
#' # unfortunately not an annotation).
#'
#' d <- data.frame(x = 1:6,
#'                 sin_x = sin(1:6),
#'                 grp = rep(c("a", "b"), 3),
#'                 stringsAsFactors = FALSE)
#'
#' # arranged by sin_x and not by grp
#' d %.>%
#'   group_by_se(., "grp") %.>%
#'   arrange_se(., "sin_x")
#'
#' # arranged by sin_x and not by grp
#' d %.>%
#'   arrange_se(., "sin_x") %.>%
#'   group_by_se(., "grp")
#'
#' # arranged by sin_x and not by grp
#' d %.>%
#'   group_by_se(., "grp") %.>%
#'   arrange_se(., "sin_x", .by_group = TRUE)
#'
#' # arranged by sin_x and not by grp
#' d %.>%
#'   arrange_se(., "sin_x", .by_group = TRUE) %.>%
#'   group_by_se(., "grp")
#'
#' @export
#'
arrange_se <- function(.data, arrangeTerms,
                       ...,
                       .by_group = FALSE,
                       strict = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "seplyr::arrange_se")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::arrange_se first argument must be a data.frame or tbl")
  }
  if(.by_group) {
    # passing the .by_group boolean to dplyr::arrange() does
    # not seem to be convenient or reliable, so simulate it by adding more
    # sorting terms.
    # https://github.com/tidyverse/dplyr/issues/3793
    gvars <- as.character(dplyr::group_vars(.data))
    arrangeTerms <- c(gvars, arrangeTerms)
  }
  nt <- length(arrangeTerms)
  if(nt>0) {
    if(!is.character(arrangeTerms)) {
      stop("seplyr::arrange_se arrangeTerms must be a character vector")
    }
    if(length(names(arrangeTerms))>0) {
      stop("seplyr::arrange_se arrangeTerms must not have names")
    }
    if(strict) {
      # confirm everything is of the form column_name or desc(column_name)
      for(i in seq_len(nt)) {
        pi <- base::parse(text = arrangeTerms[[i]])
        lp <- length(pi[[1]])
        if(lp==1) {
          # check nothing as we don't want to potentially trigger dplyr by looking at column names.
        } else if(lp==2) {
          # check we are just desc(something)
          if(as.character(pi[[1]][[1]]) != "desc") {
            stop(paste("seplyr::arrange_se term ",
                       arrangeTerms[[i]],
                       "is not in the form column_name or desc(column_name)"))
          }
        } else {
          stop(paste("seplyr::arrange_se term ",
                     arrangeTerms[[i]],
                     "is not in the form column_name or desc(column_name)"))
        }
      }
    }
    # let rlang pass the stuff on
    env = emptyenv()
    arrangeQ <- lapply(arrangeTerms,
                       function(si) {
                         rlang::parse_quosure(si,
                                              env = env)
                       })
    .data <- dplyr::arrange(.data = .data,
                            !!!arrangeQ)
  }
  .data
}
