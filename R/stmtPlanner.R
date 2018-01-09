

#' partition expressions
#'
#' Find longest ordered not created and used in same block chains.
#'
#' We assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' remaining expressions in order taking any that: have all their values available from earlier groups,
#' do not use a value formed in the current group, and do not overwrite a value formed in the current group.
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
    usedInGroup <- NULL
    formedInGroup <- NULL
    for(i in 1:n) {
      if( (de$group[[i]]<=0) &&  # available to take
          (!(de$lhs[[i]] %in% formedInGroup)) && # not assigned to in this block
          (!(de$lhs[[i]] %in% usedInGroup)) && # not used to in this block
          (length(intersect(de$deps[[i]], formedInGroup))<=0) && # not using a new value
          (length(setdiff(de$deps[[i]], have))<=0) # all pre-conditions met
      ) {
        usedInGroup <- unique(c(usedInGroup, de$syms[[i]]))
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
  # break out into mutate_se blocks
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
#' @param nexpr language item
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
    res <- unlist(lapply(nexpr, find_symbols))
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
#' We assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' remaining expressions in order taking any that: have all their values available from earlier groups,
#' do not use a value formed in the current group, and do not overwrite a value formed in the current group.
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

#' Capture the expressions of a mutate-style command.
#'
#'
#' @param ... mutate expressions with := or = used for assignment.
#' @return ordered list of mutate_se assignment blocks
#'
#' @examples
#'
#' assignments <- quote_mutate(a1 := 1, b1 = a1, a2 := 2, b2 := 7*(a1 + a2))
#' data.frame(x=1) %.>% mutate_se(., assignments)
#'
#' @export
#'
quote_mutate <- function(...) {
  mutateTerms <- substitute(list(...))
  len <- length(mutateTerms) # first slot is "list"
  if(len>1) {
    lhs <- character(len-1)
    rhs <- character(len-1)
    for(i in (2:len)) {
      ei <- mutateTerms[[i]]
      ni <- names(mutateTerms)[[i]]
      if((!is.null(ni)) && (!is.na(ni)) &&
         (is.character(ni)) && (nchar(ni)>0)) {
        vi <- paste(deparse(ei), collapse = "\n")
      } else {
        if(as.character(ei[[1]])!=':=') {
          stop("seplyr::quote_mutate terms must be of the form: sym := expr")
        }
        ni <- as.character(ei[[2]])[[1]]
        vi <- paste(deparse(ei[[3]]), collapse = "\n")
      }
      if(is.null(ni)) {
        stop("seplyr::quote_mutate terms must all have names (either from = or :=)")
      }
      lhs[[i-1]] <- ni
      rhs[[i-1]] <- vi
    }
  }
  names(rhs) = lhs
  rhs
}

#' Run a sequence of quoted mutate blocks.
#'
#' @param d data.frame to work on
#' @param blocks list of sequence named char-array of mutate blocks
#' @param env environment to work in.
#' @return d with blocks applied in order
#'
#' @examples
#'
#' plan <- partition_mutate_qt(a1 := 1, b1 := a1, a2 := 2, b2 := a1 + a2)
#' print(plan)
#' d <- data.frame(x = 1) %.>% mutate_seb(., plan)
#' print(d)
#'
#' @export
#'
mutate_seb <- function(d, blocks,
                       env = parent.frame()) {
  if(!(is.data.frame(d) || dplyr::is.tbl(d))) {
    stop("seplyr::mutate_seb first argument must be a data.frame or tbl")
  }
  for(bi in blocks) {
    d <- mutate_se(d, bi, splitTerms = FALSE, env = env)
  }
  d
}

#' Partition a sequence of mutate commands into longest ordered no create/use blocks.
#'
#' We assume the sequence of expressions is in a valid order
#' (all items available before use).  This function partitions the expressions
#' into ordered longest "no new value used blocks" by greedily scanning forward
#' remaining expressions in order taking any that: have all their values available from earlier groups,
#' do not use a value formed in the current group, and do not overwrite a value formed in the current group.
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
#' d <- data.frame(x = 1) %.>% mutate_seb(., plan)
#' print(d)
#'
#'
#' @export
#'
partition_mutate_qt <- function(...) {
  terms <- quote_mutate(...)
  lhs <- names(terms)
  rhs <- as.character(terms)
  res <- data.frame(lhs = lhs,
                    rhs = rhs,
                    stringsAsFactors = FALSE)
  syms <- lapply(res$rhs,
                 function(ei) {
                   find_symbols(parse(text = ei))
                 })
  res$syms <- syms
  partition_mutate_d(res)
}

#' Re-write a \code{dplyr::mutate()} into safe blocks.
#'
#' Note: not for use with \code{rlang} expressions (guesses variable names by text inspection).
#' See also: \url{https://winvector.github.io/rquery/articles/AssigmentPartitioner.html}.
#'
#' @param ... mutate terms
#' @param factor_mutate_warn_msg logical if TRUE issue a warning message on non-trivial mutates.
#' @return partitioned dplyr::mutate() source text
#'
#' @examples
#'
#' cat(factor_mutate(
#'  choice_a = rand_a >= 0.5,
#'    a_1 = ifelse(choice_a, 'T', 'C'),
#'    a_2 = ifelse(choice_a, 'C', 'T'),
#'   choice_b = rand_b >= 0.5,
#'    b_1 = ifelse(choice_b, 'T', 'C'),
#'    b_2 = ifelse(choice_b, 'C', 'T'),
#'   choice_c = rand_c >= 0.5,
#'    c_1 = ifelse(choice_c, 'T', 'C'),
#'    c_2 = ifelse(choice_c, 'C', 'T'),
#'   choice_d = rand_d >= 0.5,
#'    d_1 = ifelse(choice_d, 'T', 'C'),
#'    d_2 = ifelse(choice_d, 'C', 'T'),
#'   choice_e = rand_e >= 0.5,
#'    e_1 = ifelse(choice_e, 'T', 'C'),
#'    e_2 = ifelse(choice_e, 'C', 'T'),
#'  factor_mutate_warn_msg = FALSE ))
#'
#' cat(factor_mutate(
#'  choice = rand_a >= 0.5,
#'    a_1 = ifelse(choice, 'T', 'C'),
#'    a_2 = ifelse(choice, 'C', 'T'),
#'   choice = rand_b >= 0.5,
#'    b_1 = ifelse(choice, 'T', 'C'),
#'    b_2 = ifelse(choice, 'C', 'T'),
#'   choice = rand_c >= 0.5,
#'    c_1 = ifelse(choice, 'T', 'C'),
#'    c_2 = ifelse(choice, 'C', 'T'),
#'   choice = rand_d >= 0.5,
#'    d_1 = ifelse(choice, 'T', 'C'),
#'    d_2 = ifelse(choice, 'C', 'T'),
#'   choice = rand_e >= 0.5,
#'    e_1 = ifelse(choice, 'T', 'C'),
#'    e_2 = ifelse(choice, 'C', 'T'),
#'  factor_mutate_warn_msg = FALSE))
#'
#' @export
#'
factor_mutate <- function(...,
                          factor_mutate_warn_msg = TRUE) {
  plan <- partition_mutate_qt(...)
  if(factor_mutate_warn_msg && (length(plan)>1)) {
    warning("Mutate should be split into more than one stage.")
  }
  steps <- vapply(seq_len(length(plan)),
                 function(i) {
                   pi <- plan[[i]]
                   terms <- paste(names(pi), "=", pi)
                   ti <- paste(terms, collapse = ",\n          ")
                   paste0("   mutate(", ti, ")")
                 }, character(1))
  r <- paste(steps, collapse = " %>%\n")
  paste(r, "\n")
}

#' Simulate a per-row block-\code{if(){}else{}}.
#'
#' This device uses expression-\code{ifelse(,,)} to simulate the
#' more powerful per-row block-\code{if(){}else{}}.  The difference is
#' expression-\code{ifelse(,,)} can choose per-row what value to express,
#' whereas block-\code{if(){}else{}} can choose per-row where to assign multiple
#' values. By simulation we mean: a sequence of quoted mutate expressions
#' are emitted that implement the transform (versus a using a custom
#' \code{dplyr} pipe stage or function).  These expressions can then
#' be optimized into a minimal number of no-dependency
#' blocks by \code{\link{partition_mutate_se}} for efficient execution.
#' The idea is the user can write legible code in this notation, and
#' the translation turns it into safe and efficient code suitable for
#' execution either on \code{data.frame}s or at a big data scale using
#' \code{RPostgreSQL} or \code{sparklyr}.
#'
#' Note: \code{ifebtest_*}
#' is a reserved column name for this procedure.
#'
#' @param testexpr character containing the test expression.
#' @param thenexprs named character then assignments (altering columns, not creating).
#' @param elseexprs named character else assignments (altering columns, not creating).
#'
#' @examples
#'
#'  # Example: clear one of a or b in any row where both are set.
#'  d <- data.frame(a = c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1),
#'                  b = c(0, 1, 0, 1, 1, 1, 1, 1, 1, 1),
#'                  edited = FALSE)
#'
#'  program <- if_else_device(
#'    testexpr = '(a+b)>1',
#'    thenexprs = c(
#'      if_else_device(
#'        testexpr = 'runif(n()) >= 0.5',
#'        thenexprs = 'a' := '0',
#'        elseexprs = 'b' := '0'),
#'      'edited' := 'TRUE'))
#'  print(program)
#'
#'  plan <- partition_mutate_se(program)
#'  print(plan)
#'
#'  res <- d %.>%
#'    mutate_seb(., plan) %.>%
#'    select_se(., grepdf('^ifebtest_.*', ., invert=TRUE))
#'  print(res)
#'
#'  ## Note: with wrapr version 1.0.2 or greater
#'  ## you can write this without quotes code as:
#'  # program <- if_else_device(
#'  #  testexpr = qe((a+b)>1),
#'  #  thenexprs = c(
#'  #    if_else_device(
#'  #      testexpr = qe(runif(n()) >= 0.5),
#'  #      thenexprs = qae(a := 0),
#'  #      elseexprs = qae(b := 0)),
#'  #    qae(edited := TRUE)))
#'
#' @export
#'
if_else_device <- function(testexpr,
                           thenexprs = NULL,
                           elseexprs = NULL) {
  # TODO: maybe use testexpr as is when it is already a symbol.
  if((length(thenexprs) + length(elseexprs))<=0) {
    return(NULL)
  }
  knownsyms <- c(names(thenexprs), names(elseexprs))
  repeat {
    testsym <- paste0('ifebtest_',
                      as.character(paste(sample(c(letters, c(0:9)),
                                                12, replace = TRUE),
                                         collapse = '')))
    if(!(testsym %in% knownsyms)) {
      break
    }
  }
  program <- c(testsym := testexpr) # this statement is special, perculates out
  # the idea is we don't have to nest testsym generation as it is a unique
  # name, so can not be confused with other values.
  prepStmts <- function(stmts, condition) {
    ret <- NULL
    n <- length(stmts)
    if(n<=0) {
      return(ret)
    }
    isSpecial <- startsWith(names(stmts), 'ifebtest_')
    if(any(isSpecial)) {
      spc <- stmts[isSpecial]
      stmts <- stmts[!isSpecial]
      ret <- c(ret, spc)
    }
    n <- length(stmts)
    if(n<=0) {
      return(ret)
    }
    nexprs <- vapply(1:n,
                     function(i) {
                       paste0('ifelse( ', condition,
                              ', ', stmts[[i]],
                              ', ', names(stmts)[[i]],
                              ')')
                     }, character(1))
    names(nexprs) <- names(stmts)
    ret <- c(ret,nexprs)
    ret
  }
  program <- c(program,
               prepStmts(thenexprs, testsym))
  program <- c(program,
               prepStmts(elseexprs, paste('!(', testsym, ')')))
  program
}

