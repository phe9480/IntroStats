#' Sample Size calculation for achieving lower bound of CI greater than a threshold
#'
#' This function calculates the sample size required achieving lower bound of CI greater than a threshold
#'
#' @param n A range of sample size
#' @param p0 A proportion under H0
#' @param p1 Proportion under H1
#' @param conflev Confidence level, eg, 0.95
#' @param method 	Confidence interval method. Options include "Clopper-Pearson Exact", 
#' "Wilson Score", "Wilson Score with Continuity", "Asymptotic Normal", and 
#' "Asymptotic Normal with Continuity".
#'
#' @return An object with values including
#'  \itemize{
#'  \item  n.target: Target sample size achieving the lower bound >= p0
#'  \item  table: A table of r, n, CI, phat
#'  }
#'  
#' @examples
#' Example
#' n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Clopper-Pearson Exact")
#' 
#' n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Wilson Score with Continuity")
#'
#' n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Asymptotic Normal")
#' 
#' @export
#'
n.p.CI.lowerbound <- function(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Clopper-Pearson Exact"){
  
  r <- ceiling(n*p1)
  L <- U <- rep(NA, length(n))
  
  for (i in 1:length(n)){
    ci <- ci.p(r=r[i], n=n[i], conflev = conflev, method = method)
    L[i] = ci[1]; U[i] = ci[2]
  }
  
  a <- cbind(r, n, L, U)
  
  if (L[length(n)] >= p0) {
    n.target = min(n[L >= p0])
    
    #Create a range of r for the selected n
    
    r.target = ceiling(n.target * p1)
    r.range = max(0,(r.target-3)):min((r.target+3), n.target)
    ci.range = matrix(NA, nrow=length(r.range), ncol=2)
    
    for (i in 1:length(r.range)){
      ci.range[i, ] = ci.p(r=r.range[i], n=n.target, conflev = conflev, 
                           method = method)
    }
    
    b <- cbind(r.range, n.target, ci.range)
    o<-list()
    tbl = rbind(a, b)
    phat = tbl[,1]/tbl[,2]
    
    o$table <- cbind(tbl, phat)
    o$n.target <- n.target
    return(o)
  } else {stop("No solution in the provided range of n")}
  
}


