
#' Check if a string is quoted.
#'
#' @param str string to work with (not vectorized).
#' @return TRUE if string is quoted, else FALSE.
#'
#' @noRd
#'
is_quoted <- function(str) {
  # look for "'x'" forms
  nc <- nchar(str)
  if(nc<3) {
    return(FALSE)
  }
  if(!(substr(str,1,1) %in% c('"', "'"))) {
    return(FALSE)
  }
  if(substr(str,1,1)!=substr(str,nc,nc)) {
    return(FALSE)
  }
  # avoid last quote escaped situation
  # odd number of escapes prior to putative close quote.
  #  grep(regexp, '"\\\\"') # integer(0)
  #  grep(regexp, '"\\"') # integer(1)
  regexp <- '[^\\]([\\][\\])*[\\][\'|"]$'
  if(length(grep(regexp,str)==0)) {
    return(FALSE)
  }
  return(TRUE)
}

#' Remove one level of indirection from a string or name.
#'
#' Needs to return symbols to prevent re-quoting of strings.
#'
#' @param symb string or symbol to work with (not vectorized).
#' @param env environment to look in
#' @return de-quoted symb (or originval if not quoted).
#'
#' @noRd
#'
deref_symb <- function(symb, env) {
  #  look for a dereference opportunity
  val <- NULL
  tryCatch(
    val <- get(as.character(symb), envir = env),
    error = function(e) { NULL }
  )
  if((!is.null(val))&&(length(val)==1)) {
    if(is.name(val)) {
      return(val)
    }
    if(is.character(val)) {
      if(is_quoted(val)) {
        return(substr(val,2,nchar(val)-1)) # strip of quotes, but leave as string
      } else {
        return(as.name(val))
      }
    }
  }
  # return original with name xform to prevent quote growth
  if(is.name(symb)) {
    return(symb)
  }
  # character
  if(is_quoted(symb)) {
    return(substr(symb,2,nchar(symb)-1)) # strip of quotes, but leave as string
  } else {
    return(as.name(symb))
  }
}

#' Remove one level of quoting or one level of indirection from a string.
#'
#' Needs to return symbols to prevent re-quoting of strings.
#'
#' @param str string to work with (not vectorized).
#' @param env environment to look in
#' @return de-quoted str (or originval if not quoted).
#'
#' @noRd
#'
deref_str <- function(str, env) {
  # look for "'x'" forms, if so strip off one level of quotes
  if(is_quoted(str)) {
    str <- substr(str,2,nchar(str)-1)
    return(str)
  }
  # pull of implicit quotes by going to name
  return(as.name(str))
}

#' Substitute language elements by one-expand_expr.
#'
#'
#' @param lexpr language item (captured by substitute())
#' @param env environment to look in
#' @return R language element with dequotes and derefs
#'
#' @examples
#'
#' resCol1 <- "Sepal_Long"
#' ratio <- 2
#' compCol1 <- "Sepal.Width"
#' f <- function(expr, env = parent.frame()) {
#'    mt <- substitute(expr)
#'    deparse(prep_deref(mt, env))
#' }
#' expr <- f("Sepal.Length" >= ratio * compCol1)
#' print(expr)
#' iris %.>%
#'    mutate_se(., resCol1 := expr) %.>%
#'    head(.)
#'
#' @export
#'
prep_deref <- function(lexpr, env = parent.frame()) {
  nexpr <- lexpr
  n <- length(nexpr)
  # just in case (establishes an invarient of n>=1)
  if(n<=0) {
    return(nexpr)
  }
  # left-hand sides of lists/calls are represented as keys
  nms <- names(nexpr)
  if(length(nms)>0) {
    for(i in seq_len(length(nms))) {
      ki <- as.character(nms[[i]])
      if(length(ki)>0) {
        ri <- deref_str(ki, env)
        if((length(ri)>0)&&(ri!=ki)) {
          nms[[i]] <- ri
        }
      }
    }
    names(nexpr) <- nms
  }
  # establish n==1 invarient
  if(n>1) {
    for(i in seq_len(n)) {
      nexpr[[i]] <- prep_deref(nexpr[[i]], env)
    }
    return(nexpr)
  }
  # try to strip quotes off and deref
  if(is.character(nexpr)) {
    return(deref_str(nexpr, env))
  }
  if(is.name(nexpr)) {
    return(deref_symb(nexpr, env))
  }
  # fallback
  return(nexpr)
}

#' Prepare an expression for standard evaluation.
#'
#' Combine string-variable values and quoted terms to produce
#' a concrete string reading for value oriented string evaluation
#' (stringified standard evaluation).  We also call this the
#' string algebra or string interpolation.
#' This method powers the \code{seplyr::*_nse()}
#' methods and helps build string expressions without needing
#' \code{paste0())} or \code{glue::glue()}.
#'
#' @param expr to de-ref.
#' @param env enviornment in.
#' @return string
#'
#' @examples
#'
#' resCol1 <- "Sepal_Long"
#' ratio <- 2
#' compCol1 <- "Sepal.Width"
#' expr <- expand_expr("Sepal.Length" >= ratio * compCol1)
#' print(expr)
#' resCol <- 'X'
#' iris %.>%
#'    mutate_se(., resCol1 := expr) %.>%
#'    head(.)
#'
#' @export
#'
expand_expr <- function(expr, env = parent.frame()) {
  mt <- substitute(expr)
  paste(deparse(prep_deref(mt, env)), collapse = '\n')
}
