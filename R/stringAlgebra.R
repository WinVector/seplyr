

# remove one level of quoting from a string
dequote_str <- function(nexpr, env) {
  # look for "'x'" forms, if so strip off one level of quotes
  nc <- nchar(nexpr)
  regexp <- '[^\\]([\\][\\])*[\\][\'|"]$'
  # avoid last quote escaped situation
  #  grep(regexp, '"\\\\"') # integer(0)
  #  grep(regexp, '"\\"') # integer(1)
  if((nc>=3) && (substr(nexpr,1,1)==substr(nexpr,nc,nc)) &&
     (substr(nexpr,1,1) %in% c('"', "'")) &&
     (length(grep(regexp,nexpr)==0))) {
    # exception an odd number of escapes before the last quote
    return(substr(nexpr,2,nc-1))
  }
  val <- NULL
  tryCatch(
    val <- get(nexpr, envir = env),
    error = function(e) { NULL }
  )
  if((!is.null(val)) && (is.character(val)) && (length(val)==1)) {
    return(as.symbol(val))
  }
  return(as.symbol(nexpr))
}

#' Substitute language elements by one-expand_expr.
#'
#'
#' @param lexpr language item
#' @param env environment to look in
#' @return R language element with derefs
#'
#' @noRd
#'
prep_deref <- function(lexpr, env) {
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
        ri <- as.character(dequote_str(ki, env))
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
  # try to strip quotes off, producing either a symbol or string
  if(is.character(nexpr)) {
    return(dequote_str(nexpr, env))
  }
  if(is.symbol(nexpr)) { # same as is.name()
    val <- NULL
    tryCatch(
      val <- get(as.character(nexpr), envir = env),
      error = function(e) { NULL }
    )
    if((!is.null(val)) && (is.character(val)) && (length(val)==1)) {
      return(as.symbol(val))
    }
    return(nexpr)
  }
  # fallback
  return(nexpr)
}

#' Prepare an expression for standard evaluation.
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
#' head(mutate_se(iris, c(resCol := expr)))
#'
#' @export
#'
expand_expr <- function(expr, env = parent.frame()) {
  mt <- substitute(expr)
  deparse(prep_deref(mt, env))
}
