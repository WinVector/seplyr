

#' Build an anonymous function.
#'
#' Developed from:
#' \url{http://www.win-vector.com/blog/2016/12/the-case-for-using-in-r/comment-page-1/#comment-66399},
#'  \url{https://github.com/klmr/functional#a-concise-lambda-syntax},
#'  \url{https://github.com/klmr/functional/blob/master/lambda.r}
#' Called from \code{:=} operator.
#'
#' @param params formal parameters of function, unbound names.
#' @param body subsituted body of function to map arguments into.
#' @param env environment to work in
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

