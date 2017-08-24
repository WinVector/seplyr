

#' Build an anonymous function.
#'
#' Developed from:
#' \url{http://www.win-vector.com/blog/2016/12/the-case-for-using-in-r/comment-page-1/#comment-66399},
#'  \url{https://github.com/klmr/functional#a-concise-lambda-syntax},
#'  \url{https://github.com/klmr/functional/blob/master/lambda.r}
#' Called from \code{:=} operator.
#'
#' @param params formal parameters of function, unbound names.
#' @param body subsituted body of function to map arguments into (braces required for ":=" notation).
#' @param env environment to work in.
#' @return user defined function.
#'
#' @examples
#'
#' f <- makeFunction_se(as.name('x'), substitute({x*x}))
#' f(7)
#'
#' f <- x := { x*x }
#' f(7)
#'
#' g <- makeFunction_se(c(as.name('x'), as.name('y')), substitute({ x + 3*y }))
#' g(1,100)
#'
#' g <- c(x,y) := { x + 3*y }
#' g(1,100)
#'
#' @export
#'
makeFunction_se <- function(params, body, env = parent.frame()) {
  vars <- as.character(params)
  formals <- replicate(length(vars), quote(expr = ))
  names(formals) <- vars
  eval(call('function', as.pairlist(formals), body), env)
}

#' Build an anonymous function.
#'
#' Mostly just a place-holder so λ-form has somewhere safe to hang its help entry.
#'
#' @param ... formal parameters of function, unbound names, followed by function body (code/language).
#' @param body subsituted body of function to map arguments into.
#' @param env environment to work in
#' @return user defined function.
#'
#' @examples
#'
#' #lambda-syntax: lambda(arg [, arg]* [, env=env])(body)
#'
#' # example: square numbers
#' sapply(1:4, lambda(x)(x^2))
#'
#' # example more than one argumnet
#' f <- lambda(x, y)(x+y)
#' f(2,4)
#'
#' @export
#'
#'
lambda <- function(..., body = NULL, env = parent.frame()) {
  args <- substitute(list(...))
  params <- lapply(args[-1], as.name)
  force(env)
  if(!missing(body)) {
    return(makeFunction_se(params,substitute(body), env))
  }
  function(body) {
    makeFunction_se(params, substitute(body), env)
  }
}

#' @examples
#'
#' # λ-syntax: λ(arg [, arg]*, body [, env=env])
#'
#' # example: square numbers
#' sapply(1:4, λ(x, x^2))
#'
#' # example more than one argumnet
#' f <-  λ(x, y, x + 2*y)
#' f(2,4)
#'
#' # formula interface syntax: [~arg|arg(~arg)+] := body
#' f <- x~y := x + 3 * y
#' f(5, 47)
#'
#' @export
#'
#' @rdname lambda
#'
λ <- function(..., env = parent.frame()) {
  args <- substitute(list(...))
  body <- args[[length(args)]]
  args <- args[-length(args)]
  params <- lapply(args[-1], as.name)
  makeFunction_se(params, body, env)
}


#' @export
`:=.formula` <- function(args, values) {
  env = parent.frame()
  params <- setdiff(as.character(all.vars(substitute(args))),
                    '~')
  body <- substitute(values)
  makeFunction_se(params, body, env)
}
