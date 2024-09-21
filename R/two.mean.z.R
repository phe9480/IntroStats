#' Two Mean z test
#'
#' This function performs two-mean z test assuming population variances known.
#'
#' @param x1 Sample 1
#' @param x2 Sample 2
#' @param x1bar Sample 1 mean. Default NULL. If x1bar is available, then x1 is ignored.
#' @param x2bar Sample 2 mean. Default NULL. If x2bar is available, then x2 is ignored.
#' @param n1 Sample 1 size. Default NULL.
#' @param n2 Sample 2 size. Default NULL.
#' @param delta0 Population mean difference mu1-mu2 under H0. Default 0
#' @param sigma1 Population 1 standard deviation. Required
#' @param sigma2 Population 2 standard deviation. Required
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param alpha Significance level
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100 for mu1-mu2
#'  \item  xbar: Sample means
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
#' y <- x - 2*rnorm(length(x))
#'
#' two.mean.z(x1=x, x2=y, sigma1=44.89, sigma2=46.61, tail="right", alpha=0.05)
#'
#' #Use x1bar, x2bar, n1, n2
#' two.mean.z(x1bar=mean(x), n1=length(x), sigma1=44.89,
#'           x2bar=mean(y), n2=length(y), sigma2=46.61,
#'           tail="right", alpha=0.05)
#'
#' @export
two.mean.z <- function(x1, x2, x1bar=NULL, n1=NULL, sigma1,
                       x2bar=NULL, n2=NULL, sigma2,
                       tail="two", delta0=0, alpha=0.05){
  #x1bar: sample 1 mean
  #n1: sample 1 size
  #sigma1: population 1 standard deviation
  #x2bar: sample 2 mean
  #n2: sample 2 size
  #sigma2: population 2 standard deviation

  #alpha: 0.05 corresponding to 95\%CI. (1-alpha)100\%CI
  if (is.null(x1bar)) {x1bar = mean(x1)} # sample mean
  if (is.null(n1)){n1 = length(x1)} #sample size
  if (is.null(x2bar)) {x2bar = mean(x2)} # sample mean
  if (is.null(n2)){n2 = length(x2)} #sample size

  delta <- x1bar - x2bar

  se = sqrt(sigma1^2/n1+sigma2^2/n2)
  z0 = (delta - delta0)/se

  conclusion="H0 Not Rejected" #Initialized
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
  margin = qnorm(1-alpha/2)*se#margin of error
  L = delta - margin; R = delta + margin
  o = list()
  o$CI = c(L, R)
  o$mu_diff = delta; o$n = c(n1, n2); o$margin = margin; o$p=p
  o$z0 = z0; o$CV = CV; o$conclusion = conclusion;
  return(o)
}
