#' Permutation Test of One Population Mean
#'
#' This function performs the permutation test of one population mean
#' 
#' @param x sample data
#' @param mu0 mean under H0
#' @param tail type of test: "two", "left", "right"
#' @param n.perm number of permutations
#' @param save TRUE or FALSE to save test statistics from permutations
#' @param conflev Confidence level
#' @param seed seed
#' 
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval by permutation method
#'  \item  p: p value
#'  \item  T: Test statistic
#'  \item  mean: Sample mean
#'  }
#' 
#' @examples
#' x <- rnorm(10, mean=10, sd=3)
#' one.mean.perm(x=x, mu0=10, tail="two")
#' one.median.sign(x=x, m0=10, tail="two")
#' 
#' @export 
#'
one.mean.perm <- function(x, mu0, tail="two", n.perm = 5000, save=FALSE, seed = 2023, conflev=0.95){
  set.seed(seed)
  y <- x - mu0
  T <- mean(y)
  
  test.stat = rep(NA, n.perm)
  count <- 0
  for (i in 1:n.perm){
    sign <- as.numeric(runif(length(y))>0.5)
    sign[sign == 0] <- -1
    yi <- sign*abs(y)
    Ti <- mean(yi)
    if (tail == "two") {
      count <- count + (abs(Ti) >= abs(T))
    } else if (tail == "left") {
      count <- count + (Ti < T)
    } else if (tail == "right"){
      count <- count + (Ti > T)
    }
    test.stat[i] = Ti
  }
  p <- count / n.perm
  
  #Sampling based non-parametric CI
  alpha <- 1-conflev
  L <- quantile(test.stat, alpha/2)
  U <- quantile(test.stat, 1-alpha/2)
  mu_CI <- c(L, U) + mean(x) #CI for mean(X)
  
  o <- list()
  o$p = p; o$T = T; o$mean = mean(x); o$CI = mu_CI
  if (save){o$test.stat=test.stat}
  return(o)
}
  




