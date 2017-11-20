
#' rename standard interface.
#'
#' rename columns (much different syntax than \code{\link[dplyr]{rename_at}}).
#' All left hand sides are new column names and all right hand sides are old column names (
#' this allows swaps).
#'
#' Note: this method as the default setting \code{splitTerms = TRUE}, which while
#' safer (avoiding certain known \code{dplyr}/\code{dblyr} issues) can be needlessly expensive
#' and have its own "too long sequence" issues on remote-data systems
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
  res <- .data
  nMap <- length(mapping)
  if(nMap>0) {
    if(splitTerms) {
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
      badOldRefs <- setdiff(oldNames, cols)
      if(length(badOldRefs)>0) {
        stop(paste("rename_se, refering to non-existent source columns: ",
                   paste(badOldRefs, collapse = ', ')))
      }
      collisions <- intersect(newNames, setdiff(cols, oldNames))
      if(length(collisions)>0) {
        stop(paste("rename_se, new-mappings collide with original columns: ",
                   paste(collisions, collapse = ', ')))
      }
      # do the work
      for(i in sq) {
        res <- dplyr::rename(res,
                             !!rlang::sym(tmpNames[[i]]) := !!rlang::sym(oldNames[[i]]))
      }
      for(i in sq) {
        res <- dplyr::rename(res,
                             !!rlang::sym(newNames[[i]]) := !!rlang::sym(tmpNames[[i]]))
      }
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

