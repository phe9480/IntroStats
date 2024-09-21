#' One Mean z test
#'
#' This function performs one-mean Z test assuming population variance known.
#'
#' @param x A sample data set of observations
#' @param xbar Sample mean. Default NULL. If xbar is available, then x is ignored.
#' @param n Sample size. Default NULL.
#' @param mu0 Population mean under H0
#' @param sigma Population standard deviation
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param alpha Significance level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100
#'  \item  xbar: Sample mean
#'  \item  n: Sample size
#'  \item  margin: Margin of CI
#'  \item  z0: Value of z test statistic under H0
#'  \item  p: p value
#'  \item  CV: Critical value
#'  \item  conclusion: Testing result
#'  }
#'
#' @examples
#' #Example.
#' x <- c( 57.3, 57.5, 59.0, 56.5, 61.3,
#'         57.6, 59.2, 65.0, 60.1, 59.7,
#'         62.6, 52.6, 60.7, 62.3, 65.2,
#'         54.8, 55.4, 55.5, 57.8, 58.7,
#'         57.8, 60.9, 75.3, 60.6, 58.1,
#'         55.9, 61.6, 59.6, 59.8, 63.4,
#'         54.7, 60.2, 52.4, 58.3, 66.0)
#' one.mean.z(x=x, mu0=60, sigma=3.2, tail="two", alpha=0.05)
#'
#' #Use xbar
#' one.mean.z(xbar=mean(x), n = length(x), mu0=60, sigma=3.2, tail="two", alpha=0.05)
#'
#' @export
one.mean.z = function(x, xbar=NULL, n = NULL, mu0=0, sigma=1, tail="two", alpha=0.05){
  #x: a vector of random sample
  #mu0: mean under H0
  #tail: side of the test, "left", "right", "two"
  #sigma: population standard deviation
  #alpha: 0.05 corresponding to 95\%CI. (1-alpha)100\%CI
  if (is.null(xbar)) {xbar = mean(x)} # sample mean
  if (is.null(n)){n = length(x)} #sample size
  z0 = (xbar - mu0)/(sigma/sqrt(n))

  conclusion="H0 Not Rejected"
  if(tail == "left"){
    CV = qnorm(alpha);
    if(z0 < CV){conclusion="H0 Rejected"}
    p = pnorm(z0)
  }
  if(tail == "right"){
    CV = qnorm(1-alpha);
    if(z0 > CV){conclusion="H0 Rejected"}
    p = 1 - pnorm(z0)
  }
  if(tail == "two"){
    CV1 = qnorm(alpha/2);
    CV2 = qnorm(1-alpha/2);
    CV = c(CV1, CV2)
    if(z0 < CV[1] || z0 > CV[2]){conclusion="H0 Rejected"}
    p = pnorm(-abs(z0)) + 1 - pnorm (abs(z0))
  }

  #CI always 2-sided
  margin = qnorm(1-alpha/2)*sigma/sqrt(n)#margin of error
  L = xbar - margin; R = xbar + margin
  o = list()
  o$CI = c(L, R)
  o$xbar = xbar; o$n = n; o$margin = margin; o$p=p
  o$z0 = z0; o$CV = CV; o$conclusion = conclusion;
  return(o)
}


