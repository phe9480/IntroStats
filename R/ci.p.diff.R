#' Confidence Interval (Two-Sided) of two proportions using normal approximation
#'
#' This function calculates the CI of two proportions using normal approximation
#'
#' @param r1 Number of successes in sample 1
#' @param n1 Sample 1 size
#' @param x1 Sample 1. When available, re-calculate r1 and n1
#' @param r2 Number of successes in sample 2.
#' @param n2 Sample 2 size
#' @param x2 Sample 2. When available, re-calculate r2 and n2
#' @param conflev Two-sided confidence level
#'
#' @examples
#' #Example (1). x1 and x2 provided
#' x1 = sample(x=c(rep(1,30), rep(0,70)))
#' x2 = sample(x=c(rep(1,40), rep(0,60)))
#' ci.p.diff(x1=x1, x2=x2, conflev=0.95)
#'
#' #Example (2). x1 and x2 not provided
#'
#' ci.p.diff(r1 = 30, n1=100, r2=40, n2=100, conflev=0.95)
#'
#' @export
#'
ci.p.diff=function(r1, n1, r2, n2, x1=NULL, x2=NULL, conflev=0.95){
  if (!is.null(x1)){r1 = sum(x1); n1=length(x1)}
  if (!is.null(x2)){r2 = sum(x2); n2=length(x2)}

  p1 = r1/n1; p2 = r2/n2;
  alpha = 1-conflev
  za = qnorm(1-alpha/2)
  se = sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
  CI = c(p1-p2-za*se, p1-p2+za*se)
  o = list(); o$p1=p1; o$p2=p2; o$p_diff = p1-p2; o$CI=CI; o$se = se; o$za = za
  return(o)
}
