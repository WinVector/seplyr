

#' Pick a subset of rows, evaluating the subset expression
#' as if the columns of x were in the evaluation environment.
#'
#' References can be forced to the environment with a .e$ prefix
#' and forced to the data frame with a .d$ prefix (failure to
#' lookup returns null). Only works on in-memory data.frames.
#' More details here: \url{http://www.win-vector.com/blog/2018/02/is-r-basesubset-really-that-bad/}.
#'
#' @param x data.frame to work with
#' @param subset logical expression to compute per-row
#' @param env environment to work in
#' @return data.frame that is the specified subset of the rows of x.
#'
#' @seealso \code{\link[base]{subset}}
#'
#' @examples
#'
#' Temp <- 90
#' Ozone_bound <- 100
#' subset_rows(airquality,
#'             (.d$Temp > .e$Temp) &
#'               (!is.na(Ozone)) & (Ozone < Ozone_bound))
#'
#' @export
#'
subset_rows <- function(x, subset, env = parent.frame()) {
  if(!is.data.frame(x)) {
    stop("subset_rows expected x to be a data.frame")
  }
  if(missing(subset) || (nrow(x)<=0)) {
    return(x)
  }
  e <- substitute(subset) # capture expression
  eval_env <- new.env(parent = env)
  assign(".d", x, eval_env)   # data prefix .d$
  assign(".e", env, eval_env) # environment prefix .e$
  r <- eval(e, envir = x, enclos = eval_env)
  if(!is.logical(r)) {
    stop("subset_rows predicate must evaluate to logical")
  }
  r <- r & !is.na(r)
  if(length(r)!=nrow(x)) {
    stop("subset_rows predicate must have one entry per row")
  }
  x[r, , drop = FALSE]
}

# also think about transform, and with (probably not within)


