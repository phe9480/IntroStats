#' Permutation Tests comparing two population means
#'
#' This function performs the permutation test of comparing two population means
#'  
#' @param x1 sample 1
#' @param x2 sample 2
#' @param conflev Confidence level
#' @param tail type of test "two", "left", "right". Default "two"
#' @param n.perm number of permutations, default 10000
#' @param seed default 2023
#' 
#' 
#' @return An object having values including
#'  \itemize{
#'  \item  p: p value
#'  \item  mean_diff: Sample mean difference
#'  \item  mean_diff_CI: confidence interval for mu1 - mu2
#'  }
#' 
#' @examples
#' #Example
#' set.seed(2023)
#' x1 <- rnorm(50, mean=10, sd=2)
#' x2 <- rnorm(30, mean=12, sd=2.5)
#' two.mean.perm(x1, x2, tail="two", conflev=0.95, n.perm=10000, seed=2023)
#' 
#' 
#' @export 
#'
two.mean.perm <- function(x1, x2, tail="two", conflev=0.95,
                          n.perm=10000, seed=2023){
  #x1: group 1 observations
  #x2: group 2 observations
  #conflev: Confidence Interval level for mean difference
  
  #Define the test statistic as the interest of test: mean
  #T = mean(x1) - mean(x2)
  
  #Define a group variable
  group <- c(rep(1, length(x1)), rep(2, length(x2)))
  x <- c(x1, x2)
  
  #Create a data frame
  dat <- as.data.frame(cbind(x, group))
  
  #Observed value of the test statistic
  T0 <- mean(x1) - mean(x2)
  
  #Sampling group variable and 
  #Determine the empirical distribution of the test statistic T
  set.seed(seed)
  T <- rep(NA, n.perm)
  
  for (i in 1:n.perm){
    groupi <- sample(group) #Sampling without replacement
    x1i <- x[groupi == 1]   #New group assignment
    x2i <- x[groupi == 2]
    T[i] <- mean(x1i) - mean(x2i) #Test statistic value under H0
  }
  
  #Calculate the p value based on empirical distribution of T
  #count number of test statistics at least as extreme as T0
  if (tail == "two"){
    extremes = sum(abs(T) >= abs(T0))
  } else if (tail == "left"){
    extremes = sum(T <= T0)
  } else if (tail == "right"){
    extremes = sum(T >= T0)
  }
  
  #Calculate the p value
  p = extremes / n.perm
  
  #Non-parametric confidence interval
  alpha <- 1 - conflev
  L <- T0+quantile(T, alpha/2); 
  R <- T0+quantile(T, 1-alpha/2)
  
  o <- list()
  o$p <- p; o$mean_diff <- T0; o$mean_diff_CI <- c(L, R)
  return(o)
}


