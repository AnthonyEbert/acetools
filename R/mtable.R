#' Create frequency table from multivariate data.
#' @description How many times does a unique combination of variables occur?
#' @param x matrix or dataframe
#' @importFrom magrittr %>%
#' @export
mtable <- function(x){
  if(class(x) == "matrix"){
    x <- as.data.frame(x)
  } else if(!("data.frame" %in% class(x))){
    stop("Needs to be a matrix or data.frame")
  }

  x$hashes <- factor(apply(x, 1, digest::digest))

  thashes <- table(x$hashes)

  thashes_df <- data.frame(hashes = names(thashes), freq = as.numeric(thashes))

  output <- dplyr::left_join(x, thashes_df, by = "hashes") %>% dplyr::pull(freq)

  return(output)
}
