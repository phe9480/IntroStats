#' Simultaneous CIs Calculation using Tukey Method in One-Way ANOVA 
#' 
#' This function calculates the pairwise simultaneous CIs using Tukey Method 
#' in One-Way ANOVA 
#'
#' @param data Input data in data dump format, for example, 
#' data = list(x1=c(5, 4, 6), x2=c(7, 8, 4), x3 = c(9, 10, 15, 8)) 
#' where xi is the ith sample in one-way anova analysis.
#' @param conflev Confidence interval level, e.g., 0.95
#'
#' @return An object with values including
#'  \itemize{
#'  \item  mu.diff: a matrix of pairwise sample mean difference
#'  \item  CI: simultaneous pairwise CI for mu_i - mu_j, which is CI[i,j,].
#'  }
#'
#' @examples
#' 
#' data = list(x1=c(5, 4, 6), x2=c(7, 8, 4), x3 = c(9, 10, 15, 8))
#' anova_simCIs(data, conflev=0.95)
#'
#' @export
#' 
#' 
anova_simCIs <- function(data, conflev=0.95){

  mse = anova_MSE(data)
  k = mse$k #number of groups
  n = mse$n; mu = mse$mu
  
  q = qtukey(conflev, nmeans=mse$k, df=mse$N-k)
  CI = array(NA, dim=c(k,k,2))
  L = R = mu.d = matrix(NA, nrow=k, ncol=k)
  
  for (i in 1:(k-1)) {
    for (j in (i+1):k){
      margin = q*sqrt(mse$MSE)/sqrt(2)*sqrt(1/n[i]+1/n[j])
      mu.diff = mu[i] - mu[j]
      L[i, j] = mu.diff - margin
      R[i, j] = mu.diff + margin
      CI[i, j, ] = c(L[i, j], R[i,j])
      mu.d[i,j] = mu.diff
    }
  }
  o = list()
  o$mu.diff = mu.d
  o$CI = CI
  return(o) 
}

