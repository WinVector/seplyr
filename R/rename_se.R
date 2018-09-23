
#' rename standard interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename_at}}).
#' All left hand sides are new column names and all right hand sides are old column names (
#' this allows swaps).
#'
#' Note: this method as the default setting \code{splitTerms = TRUE}, which is
#' safer (avoiding certain known \code{dplyr}/\code{dblyr} issues)
#' (please see the side-notes of \url{http://winvector.github.io/FluidData/partition_mutate.html} for some references).
#'
#' @seealso \code{\link[dplyr]{rename}},  \code{\link[dplyr]{rename_at}}, \code{\link[wrapr]{:=}}
#'
#' @param .data data.frame
#' @param mapping named character vector of columns to rename (new names on the left, original names on the right; this may seem reversed but it matches dplyr::rename()).
#' @param splitTerms logical, if TRUE into separate renames (if FALSE instead, pass all at once to dplyr).
#' @param env environment to work in.
#' @return .data with renamed columns
#'
#' @examples
#'
#'
#' datasets::mtcars %.>%
#'    rename_se(., c("cylinders" := "cyl", "gears" := "gear")) %.>%
#'    head(.)
#' # # same as:
#' # datasets::mtcars %>%
#' #    rename(cylinders = cyl, gears = gear) %>%
#' #    head()
#'
#' # rename_se allows column swaps
#' data.frame(a = 1, b = 2) %.>%
#'    rename_se(., c('a', 'b') := c('b', 'a'))
#'
#' @export
#'
rename_se <- function(.data, mapping,
                      splitTerms = TRUE,  env = parent.frame()) {
  if(!(is.data.frame(.data) || dplyr::is.tbl(.data))) {
    stop("seplyr::rename_se first argument must be a data.frame or tbl")
  }
  if(!is.character(mapping)) {
    stop("seplyr::rename_se mapping must be a named character vector")
  }
  force(env)
  res <- .data
  nMap <- length(mapping)
  if(nMap>0) {
    if(splitTerms && (length(intersect(as.character(mapping), names(mapping)))>0)) {
      sq <- seq_len(nMap)
      # get task
      cols <- colnames(.data)
      oldNames <- as.character(mapping)
      newNames <- names(mapping)
      tmpNames <- setdiff(paste('RENAMETMPCOL', sq, sep='_'),
                          c(cols,
                            oldNames, newNames))[sq]
      # some minor checking
      if(length(unique(newNames))!=nMap) {
        stop("rename_se: named new column more than once")
      }
      if(length(unique(oldNames))!=nMap) {
        stop("rename_se: named original column more than once")
      }
      mapping1 <- oldNames
      names(mapping1) <- tmpNames
      mapping2 <- tmpNames
      names(mapping2) <- newNames
      retnameQ1 <- lapply(mapping1,
                         function(si) {
                           rlang::sym(si)
                         })
      names(retnameQ1) <- names(mapping1)
      retnameQ2 <- lapply(mapping2,
                          function(si) {
                            rlang::sym(si)
                          })
      names(retnameQ2) <- names(mapping2)
      res <- dplyr::rename(.data = dplyr::rename(.data = res, !!!retnameQ1), !!!retnameQ2)
    } else {
      # pass directly to dplyr
      retnameQ <- lapply(mapping,
                        function(si) {
                          rlang::sym(si)
                        })
      names(retnameQ) <- names(mapping)
      res <- dplyr::rename(.data = res, !!!retnameQ)
    }
  }
  res
}

