#' aaply is like sapply but guaranteed to return a matrix
#' @param X a vector (atomic or list) or an expression object. Other objects (including classed objects) will be coerced by base::as.list.
#' @param FUN the function to be applied to each element of X. In the case of functions like +, %*%, the function name must be backquoted or quoted.
#' @param ... option arguments to \code{FUN}
#' @export
aapply <- function(X, FUN, ...){
  x <- sapply(X, FUN, ...)
  if(class(x) != "matrix"){
    x <- t(x)
  }
  return(x)
}
