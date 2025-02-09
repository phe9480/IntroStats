#' Two sd F Test
#'
#' This function performs two sd F test for comparing two population sd.
#'
#' @param s1 Sample 1 sd. 
#' @param n1 Sample size. 
#' @param s2 Sample 2 sd. 
#' @param n2 Sample size. 
#' @param alpha Significance level
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param conflev confidence interval level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100
#'  \item  F0: Value of F test statistic under H0
#'  \item  p: p value
#'  \item  CV: Critical value
#'  \item  conclusion: Testing result
#'  }
#'
#' @examples
#' two.sd.F(alpha=0.05, s1=30.5, n1=8, s2=64.5, n2=15, tail="left", conflev=0.9)
#'
#'
#' @export
#' 
two.sd.F = function(alpha, s1, n1, s2, n2, tail="left", conflev=0.95){
  a = alpha
  F0 = s1^2/s2^2
  conclusion="H0 Not Rejected"
  if(tail == "left"){
    CV = qf(a, df1=n1-1, df2=n2-1);if(F0 < CV){conclusion="H0 Rejected"}
    p = pf(F0, df1=n1-1, df2=n2-1)}
  if(tail == "right"){
    CV = qf(1-a, df1=n1-1, df2=n2-1); if(F0 > CV){conclusion="H0 Rejected"}
    p = 1-pf(F0, df1=n1-1, df2=n2-1)
    }
  if(tail == "two"){
    CV1 = qf(a/2, df1=n1-1, df2=n2-2);
    CV2 = qf(1-a/2, df1=n1-1, df2=n2-1); 
    CV = c(CV1, CV2)
    if(F0 < CV[1] || F0 > CV[2]){conclusion="H0 Rejected"} 
    p = 2*min(pf(F0, df1=n1-1, df2=n2-1), 1-pf(F0, df1=n1-1, df2=n2-1))
    }
  
  #always 2-sided CI for sigma1/sigma2
  F.left = qf((1-conflev)/2, df1=n1-1, df2=n2-1)
  F.right = qf(1-(1-conflev)/2, df1=n1-1, df2=n2-1)
  CI=s1/s2*c(1/sqrt(F.right), 1/sqrt(F.left))
  o = list(); o$F0 = F0; o$CV = CV; 
  o$conclusion = conclusion; o$CI=CI; 
  o$p = p;
  o$F.left=F.left; o$F.right=F.right
  return(o)
}

