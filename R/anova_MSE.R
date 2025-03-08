#' MSE Calculation in One-Way ANOVA 
#' 
#' This function calculates the SSE and MSE in one-way ANOVA.
#'
#' @param data Input data in data dump format, for example, 
#' data = list(x1=c(5, 4, 6), x2=c(7, 8, 4), x3 = c(9, 10, 15, 8)) 
#' where xi is the ith sample in one-way anova analysis.
#'
#' @return An object with values including
#'  \itemize{
#'  \item  n: sample size in each sample
#'  \item  N: total sample size
#'  \item  k: number of samples
#'  \item  mu: sample mean in each sample
#'  \item  s: sample standard deviation in each sample
#'  \item  SSE: sum of squared error
#'  \item  MSE: mean squared error
#'  }
#'
#' @examples
#' data = list(x1=c(5, 4, 6), x2=c(7, 8, 4), x3 = c(9, 10, 15, 8))
#' anova_MSE(data)
#'
#' @export
#' 
anova_MSE = function(data){
  k = length(data)  #Number of populations, k
  s2 = n = rep(NA, k) #s2 is sample variance
  for (i in 1:k){s2[i] = var(data[[i]])}
  n = lengths(data)
  
  SSE = sum((n-1)*s2)
  MSE = SSE / (sum(n) - k)
  
  #mu and s
  mu = s = rep(NA, k)
  for (i in 1:k){
    mu[i] = mean(data[[i]])
    s[i] = sqrt(var(data[[i]]))
  }
  
  o = list()
  o$n = n; o$N = sum(n); o$k=k; o$mu = mu; o$s = s
  o$SSE = SSE; o$MSE = MSE
  return(o)    
}
