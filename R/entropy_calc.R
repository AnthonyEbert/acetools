#' Calculate entropy of list or vector
#' @param x list or vector
#' @param naive logical use naive algorithm for entropy calculation
#' @param hash logical use hash
#' @examples
#' x <- mtcars[,c(8:11)]
#' y <- split(sapply(x, function(x){x}), 1:32)
#' entropy_calc(y, hash = TRUE)
#' @export
entropy_calc <- function(x, naive = FALSE, hash = FALSE){
  if(hash){
    x <- sapply(x, digest::digest)
  }

  C_hat <- 1 - sum(nonduplicated(x)) / length(x)
  if(naive){
    C_hat <- 1
  }
  p_hat <- as.numeric(prop.table(table(x)))
  p_tilde <- C_hat * p_hat
  entropy <- - sum( (p_tilde * log(p_tilde)) / (1 - (1 - p_tilde)^length(x)) )

  return(entropy)
}

#' @export
nonduplicated <- function(x){

  return(!(duplicated(x) | duplicated(x, fromLast = TRUE)))
}
