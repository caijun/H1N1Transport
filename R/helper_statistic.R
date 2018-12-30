#' @references \url{https://stats.stackexchange.com/questions/21103/confidence-interval-for-median}
#' @param x a vector
#' @param stat a function that produces the statistics to be bootstrapped, 
#' default is median
#' @param n number of bootstrap replicates 
#' @usage 
#' bootmin <- boot.stat(x, stat = min, n = 10^4)
#' quantile(bootmin, c(.025, 0.5, 0.975))
boot.stat <- function(x, stat = median, n = 10^4) {
  res <- apply(matrix(sample(x, replace = TRUE, n*length(x)), nrow = n), 
               1, stat)
  return(res)
}