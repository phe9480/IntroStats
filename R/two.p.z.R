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
#' two.p.z(x1=21, n1=150, x2=48, n2=200, tail="two", alpha=0.05, conflev=0.95)
#'
#' @export
two.p.z <- function(x, y, x1=NULL, n1=NULL, x2=NULL, n2=NULL, tail="left",
                 alpha=0.05, conflev=0.95){

  if(is.null(x1)){x1 <- sum(x)}
  if(is.null(x2)){x2 <- sum(y)}
  if(is.null(n1)){n1 <- length(x)}
  if(is.null(n2)){n2 <- length(y)}

  p1 = x1/n1; p2= x2/n2; pp = (x1+x2)/(n1+n2)
  z0 = (p1-p2)/sqrt(pp*(1-pp)*(1/n1+1/n2))
  conclusion="H0 Not Rejected"
  if(tail == "left"){
    CV = qnorm(alpha);
    if(z0 < CV){conclusion="H0 Rejected"}
    p <- pnorm(z0)
  }
  if(tail == "right"){
    CV = qnorm(1-alpha);
    if(z0 > CV){conclusion ="H0 Rejected"}
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
  se = sqrt(p1*(1-p1)/n1+p2*(1-p2)/n2)
  CI = c(p1-p2-za*se, p1-p2+za*se)
  o = list(); o$z0 = z0; o$CV = CV;
  o$conclusion = conclusion; o$CI=CI;
  o$p = p;
  o$pp = pp
  o$phat <- c(p1, p2)
  return(o)
}
