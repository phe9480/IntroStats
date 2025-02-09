#' One sd chi-square test
#'
#' This function performs one-sd chi-square test
#'
#' @param s Sample sd. 
#' @param n Sample size. 
#' @param alpha Significance level
#' @param sd0 Population sd under H0
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param conflev confidence interval level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100
#'  \item  chisq0: Value of chi-square test statistic under H0
#'  \item  p: p value
#'  \item  CV: Critical value
#'  \item  conclusion: Testing result
#'  }
#'
#' @examples
#' x <- c(120.94, 118.58, 119.41, 120.23, 121.13, 118.22, 119.71, 121.09, 120.56, 119.11)
#' one.sd.chisq.test(s=sd(x), n=length(x), alpha=0.05, sd0=4, tail="two", conflev = 0.95)
#'
#'
#' @export
one.sd.chisq.test = function(s, n, alpha=0.05, sd0, tail="left", conflev = 0.95){
  #value of the test statistic
  chisq0 = (n-1)*s^2 / sd0^2
  conclusion="H0 Not Rejected"
  #critical values
  if(tail=="left"){
    CV = qchisq(p=alpha, df=n-1); 
    if(chisq0 < CV){conclusion="H0 Rejected"}
    p = pchisq(chisq0, df=n-1)
    }
  if(tail=="right"){
    CV = qchisq(p=1-alpha, df=n-1); 
    if(chisq0 > CV){conclusion="H0 Rejected"}
    p = 1-pchisq(chisq0, df=n-1)
    }
  if(tail=="two"){
    CV = c(qchisq(p=alpha/2, df=n-1), qchisq(p=1-alpha/2, df=n-1))
  if(chisq0 < CV[1] || chisq0 > CV[2]){conclusion="H0 Rejected"}
    p = 2*min(pchisq(chisq0, df=n-1),1-pchisq(chisq0, df=n-1))
  }
  #always 2-sided CI for sd
  CI.left = s*sqrt(n-1)/sqrt(qchisq(p=1-(1-conflev)/2, df=n-1))
  CI.right = s*sqrt(n-1)/sqrt(qchisq(p=(1-conflev)/2, df=n-1))
  CI = c(CI.left, CI.right)
  
  o = list(); o$chisq0 = chisq0; o$CV = CV; o$p = p; o$conclusion = conclusion; o$CI=CI  
  return(o)
}
