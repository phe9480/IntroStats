#' One sample proportion test
#'
#' This function performs one sample proportion test using normal approximation.
#'
#' @param x A binary sample data set of observations and each element having a value of 0 or 1.
#' @param phat Sample proportion. Default NULL. If phat is available, then x is ignored.
#' @param n Sample size. Default NULL.
#' @param p0 Population mean under H0, default 0
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param alpha Significance level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100
#'  \item  phat: Sample proportion
#'  \item  n: Sample size
#'  \item  margin: Margin of CI
#'  \item  z0: Value of z test statistic under H0
#'  \item  p: p value
#'  \item  CV: Critical value
#'  \item  se: Standard error
#'  \item  conclusion: Testing result
#'  }
#'
#' @examples
#' #Example.
#' x <- c(rep(1, 24), rep(0, 301-24))
#' one.p.z(x=x, p0=0.063, tail="right", alpha=0.05)
#'
#' #Use summary statistics
#' one.p.z(phat=mean(x), n = length(x), p0=0.063, tail="right", alpha=0.05)
#'
#' @export
one.p.z=function(x, phat=NULL, n=NULL, p0=0, tail="left", alpha=0.05, conflev=0.95){
  if (is.null(phat)){phat <- mean(x)}
  if (is.null(n)){n <- length(x)}

  z0 = (phat-p0)/sqrt(p0*(1-p0)/n)
  conclusion="H0 Not Rejected"
  if(tail == "left"){
    CV = qnorm(alpha);
    if(z0 < CV){conclusion="H0 Rejected"}
    p <- pnorm(z0)
  }
  if(tail == "right"){
    CV = qnorm(1-alpha);
    if(z0 > CV){conclusion="H0 Rejected"}
    p <- 1-pnorm(z0)
  }
  if(tail == "two"){
    CV1 = qnorm(alpha/2);
    CV2 = qnorm(1- alpha/2);
    CV = c(CV1, CV2)
    if(z0 < CV[1] || z0 > CV[2]){conclusion="H0 Rejected"}
    p <- 2*pnorm(-abs(z0))
  }

  #always 2-sided CI
  za = qnorm(1-(1-conflev)/2)
  se = sqrt(phat*(1-phat)/n)
  CI = c(phat-za*se, phat+za*se)
  o = list(); o$phat=phat; o$z0 = z0; o$CV = CV; o$conclusion = conclusion;
  o$CI=CI; o$se=se; o$n=n; o$p=p
  return(o)
}
