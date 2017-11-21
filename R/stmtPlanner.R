

#' partition expressions
#'
#' Find longest ordered not created and used in same block chains.
#'
#' The idea is: we assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' and skipping any expressions that either use a value created in the current block
#' or require a value not yet produced.
#'
#' @param de frame of expressions
#' @return ordered list of mutate_se assignment blocks
#'
#' @noRd
#'
partition_mutate_d <- function(de) {
  n <- nrow(de)
  # find step to step dependences
  mostRecent <- list()
  deps <- vector(mode = 'list', length = n)
  for(i in 1:n) {
    si <- de$syms[[i]]
    depsi <- numeric(0)
    dkeys <- intersect(si, names(mostRecent))
    if(length(dkeys)>0) {
      depsi <- as.numeric(mostRecent[dkeys])
    }
    deps[[i]] <- depsi
    mostRecent[de$lhs[[i]]] <- i
  }
  de$deps <- deps
  de$origOrder = 1:n
  de$group <- 0L
  group <- 1L
  while(any(de$group<=0)) {
    # sweep forward in order greedily taking anything
    have <- which(de$group>0)
    formedInGroup <- NULL
    for(i in 1:n) {
      if( (de$group[[i]]<=0) &&  # available to take
         (length(intersect(de$deps[[i]], formedInGroup))<=0) && # not using a new value
         (length(setdiff(de$deps[[i]], have))<=0) # all pre-conditions met
         ) {
        formedInGroup <- c(formedInGroup, de$lhs[[i]])
        de$group[[i]] <- group
      }
    }
    if(length(formedInGroup)<=0) {
      # should only get here in error
      # but if we don't stop we will spin forever
      stop("seplyr::partition_mutate_d pass failed to accumulate steps")
    }
    group <- group + 1L
  }
  de <- de %.>%
    arrange_se(., c("group", "origOrder"))
  # break out into mutate_se blocks (crude split)
  res <- rep(list(character(0)), max(de$group))
  gnames <- paste0('group', sprintf("%05g", 1:max(de$group)))
  names(res) <- gnames
  for(i in 1:n) {
    gi <- gnames[[de$group[[i]]]]
    res[[gi]] <- c(res[[gi]], de$lhs[[i]] := de$rhs[[i]])
  }
  res
}


#' Scan for symbols.
#'
#' @param lexpr language item
#' @return R language element with substitutions
#'
#' @noRd
#'
find_symbols <- function(nexpr) {
  n <- length(nexpr)
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return(NULL)
  }
  # basic recurse, establish invariant n==1
  if(n>1) {
    if(is.call(nexpr)) {
      nexpr <- nexpr[-1]
      if(length(nexpr)<=0) {
        return(NULL)
      }
    }
    res <- lapply(nexpr, find_symbols)
    res <- Filter(function(ri) {!is.null(ri)}, res)
    return(as.character(res))
  }
  if(is.expression(nexpr)) {
    return(find_symbols(nexpr[[1]]))
  }
  # this is the main re-mapper
  if(is.symbol(nexpr)) { # same as is.name()
    return(as.character(nexpr))
  }
  # fall-back
  return(NULL)
}



#' Partition a sequence of mutate commands into longest ordered no create/use blocks.
#'
#' The idea is: we assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' and skipping any expressions that either use a value created in the current block
#' or require a value not yet produced.
#' For an example please see \url{http://winvector.github.io/FluidData/partition_mutate.html}.
#'
#' @param exprs list of source-text of a sequence of mutate expressions.
#' @return ordered list of mutate_se assignment blocks
#'
#' @examples
#'
#' partition_mutate_se(c("a1" := "1", "b1" := "a1", "a2" := "2", "b2" := "a1 + a2"))
#'
#' @export
#'
partition_mutate_se <- function(exprs) {
  res <- data.frame(lhs = names(exprs),
                    rhs = as.character(exprs),
                    stringsAsFactors = FALSE)
  res$syms <- lapply(res$rhs,
                     function(ei) {
                       find_symbols(parse(text = ei))
                     })
  partition_mutate_d(res)
}


#' Partition a sequence of mutate commands into longest ordered no create/use blocks.
#'
#' The idea is: we assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' and skipping any expressions that either use a value created in the current block
#' or require a value not yet produced.
#' For an example please see \url{http://winvector.github.io/FluidData/partition_mutate.html}.
#'
#' Note: unlike \code{\link{mutate_nse}} \code{partition_mutate_qt} does not perform
#' substitutions.
#'
#' @param ... mutate expressions with := used for assignment.
#' @return ordered list of mutate_se assignment blocks
#'
#' @examples
#'
#' plan <- partition_mutate_qt(a1 := 1, b1 := a1, a2 := 2, b2 := a1 + a2)
#' print(plan)
#' d <- data.frame(x = 1)
#' for(si in plan) {
#'    print(si)
#'    d <- mutate_se(d, si)
#' }
#' print(d)
#'
#'
#' @export
#'
partition_mutate_qt <- function(...) {
  mutateTerms <- substitute(list(...))
  len <- length(mutateTerms) # first slot is "list"
  if(len>1) {
    lhs <- character(len-1)
    rhs <- character(len-1)
    syms <- vector(mode = 'list', length=len-1)
    for(i in (2:len)) {
      ei <- mutateTerms[[i]]
      if((length(ei)!=3)||(as.character(ei[[1]])!=':=')) {
        stop("partition_mutate_qt terms must be of the form: sym := expr")
      }
      lhs[[i-1]] <- as.character(ei[[2]])[[1]]
      syms[[i-1]] <- find_symbols(ei[[3]])
      rhs[[i-1]] <- paste(deparse(ei[[3]]), collapse = "\n")
    }
  }
  res <- data.frame(lhs = lhs,
                    rhs = rhs,
                    stringsAsFactors = FALSE)
  res$syms <- syms
  partition_mutate_d(res)
}

