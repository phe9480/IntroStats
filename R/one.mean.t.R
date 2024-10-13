#' One Mean t test
#'
#' This function performs one-mean t test assuming population variance unknown.
#'
#' @param x A sample data set of observations
#' @param xbar Sample mean. Default NULL. If xbar is available, then x is ignored.
#' @param n Sample size. Default NULL.
#' @param mu0 Population mean under H0
#' @param s Sample standard deviation
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param alpha Significance level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100
#'  \item  xbar: Sample mean
#'  \item  n: Sample size
#'  \item  s: Sample standard deviation
#'  \item  margin: Margin of CI
#'  \item  t0: Value of t test statistic under H0
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
#' one.mean.t(x=x, mu0=60, tail="two", alpha=0.05)
#'
#' #Use xbar, n, and s
#' one.mean.t(xbar=mean(x), n = length(x), s=sd(x), mu0=60, tail="two", alpha=0.05)
#'
#' @export
one.mean.t = function(x, xbar=NULL, n=NULL, s = NULL, mu0=0, tail="two", alpha=0.05){
  #x: a vector
  #mu0: mean under H0
  #tail: side of the test, "left", "right", "two"
  #alpha: 0.05 corresponding to 95\%CI. (1-alpha)100\%CI

  if (is.null(xbar)) {xbar = mean(x)} # sample mean
  if (is.null(n)){n = length(x)} #sample size
  if (is.null(s)){s = sd(x)}
  t0 = (xbar - mu0)/(s/sqrt(n))
  conclusion="H0 Not Rejected"
  if(tail == "left"){
    CV = qt(alpha, df=n-1);
    if(t0 < CV){conclusion="H0 Rejected"}
    p = pt(t0, df=n-1)
  }
  if(tail == "right"){
    CV = qt(1-alpha, df=n-1);
    if(t0 > CV){conclusion="H0 Rejected"}
    p = 1-pt(t0, df=n-1)
  }
  if(tail == "two"){
    CV1 = qt(alpha/2, df=n-1);
    CV2 = qt(1-alpha/2, df=n-1);
    CV = c(CV1, CV2)
    if(t0 < CV[1] || t0 > CV[2]){conclusion="H0 Rejected"}

    p = 1-pt(abs(t0), df=n-1) + pt(-abs(t0), df=n-1)
  }
  #CI always 2-sided
  margin = qt(1-alpha/2, df=n-1)*s/sqrt(n)#margin of error
  L = xbar - margin; R = xbar + margin
  o = list()
  o$CI = c(L, R)
  o$xbar = xbar; o$n = n; o$margin = margin; o$s = s; o$p=p
  o$t0 = t0; o$CV = CV; o$df=n-1; o$conclusion = conclusion;
  return(o)
}
