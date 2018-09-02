
#' arrange standard interface.
#'
#' Arrange a data frame by the arrangeTerms.  Accepts arbitrary text as
#' arrangeTerms to allow forms such as "desc(gear)". Intent is to arrange only by
#' sets of variables or desc() reversals, not by arbitrary expressions over variables.
#' To help enforce this parsing is performed in an empty environment (so expressions
#' such as "gear + carb" deliberately error-out).
#'
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{arrange_at}}
#'
#' @param .data data.frame
#' @param arrangeTerms character vector of column expressions to arrange by.
#' @param ... not used, force later arguments to bind by name.
#' @param strict logical if TRUE accept only name and desc(name) terms.
#' @return .data arrnaged by arrangeTerms
#'
#' @examples
#'
#' datasets::mtcars %.>%
#'   arrange_se(., c("cyl", "desc(gear)")) %.>%
#'   head(.)
#' # equivilent to:
#' # arrange(datasets::mtcars, cyl, desc(gear)) %>% head()
#'
#' @export
#'
arrange_se <- function(.data, arrangeTerms,
                       ...,
                       strict = TRUE) {
  wrapr::stop_if_dot_args(substitute(list(...)),
                          "seplyr::arrange_se")
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::arrange_se first argument must be a data.frame or tbl")
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
    .data <- dplyr::arrange(.data = .data, !!!arrangeQ)
  }
  .data
}
