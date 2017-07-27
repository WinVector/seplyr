
#' Generate a name with a prefix disjoint from a set of names
#'
#' @param prefix character, desired prefix
#' @param names character list of names to avoid
#' @return new name disjoint from set of names
#'
#' @examples
#'
#' # basic op
#' novelName('b', c('a', 'b', 'c'))
#'
#' # complex application (converting logistic
#' # links to probabilities).
#' d <- data.frame(
#'   exampleId = c(1, 1, 2, 2),
#'   resultLabel = c('a', 'b' , 'a', 'b'),
#'   linkValue = c(-5, 2, -2, -1),
#'   stringsAsFactors = FALSE)
#' totColName <- novelName('t', colnames(d))
#' d ->.;
#'   mutate_se(., c(totColName := "exp(linkValue)")) ->.;
#'   group_by_se(., "exampleId") ->.;
#'   mutate_se(., c("probability" :=
#'                 paste0(totColName, '/sum(', totColName, ')'))) ->.;
#'   deselect(., totColName)
#'
#' @export
#'
novelName <- function(prefix, names) {
  if(!(prefix %in% names)) {
    return(prefix)
  }
  setdiff(paste(prefix, seq_len(length(names)+1), sep = '_'),
          names)[[1]]
}

