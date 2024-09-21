#' Two Mean t test
#'
#' This function performs two-mean t test assuming population variances unknown.
#'
#' @param x1 Sample 1
#' @param x2 Sample 2
#' @param x1bar Sample 1 mean. Default NULL. If x1bar is available, then x1 is ignored.
#' @param x2bar Sample 2 mean. Default NULL. If x2bar is available, then x2 is ignored.
#' @param n1 Sample 1 size. Default NULL.
#' @param n2 Sample 2 size. Default NULL.
#' @param s1 Sample 1 sd. Default NULL.
#' @param s2 Sample 2 sd. Default NULL.
#' @param delta0 Population mean difference mu1-mu2 under H0. Default 0
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' @param alpha Significance level
#' @param equal.variance TRUE or FALSE
#' @param uneqvar.method "Welch" or "Cochran". This option is applicable only
#' in the scenario of unequal variance
#'
#' @return An object with values including
#'  \itemize{
#'  \item  CI: Confidence interval (1-alpha)*100 for mu1-mu2
#'  \item  xbar: Sample means
#'  \item  n: Sample size
#'  \item  s: Sample sd
#'  \item  sp: Pooled sample sd
#'  \item  se: Sample se when using unpooled method
#'  \item  margin: Margin of CI
#'  \item  t0: Value of t test statistic under H0
#'  \item  df: Degree of freedom for t test
#'  \item  p: p value
#'  \item  CV: Critical value
#'  \item  conclusion: Testing result
#'  \item  equal.variance: Variance assumption
#'  \item  uneqvar.method: Method for handling unequal variance in t test
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
#' two.mean.t(x1=x, x2=y, s1=sd(x), s2=sd(y), tail="right", alpha=0.05)
#'
#' #Use summary statistics x1bar, x2bar, n1, s1, n2, s2
#' two.mean.t(x1bar=59.01, n1=53, s1=44.89,
#'                      x2bar=46.61, n2=54, s2=34.85,
#'                      tail="right", alpha=0.05)
#'
#' two.mean.t(x1bar=59.01, n1=53, s1=44.89,
#'                      x2bar=46.61, n2=54, s2=34.85,
#'                      tail="right", alpha=0.05,
#'                      equal.variance=FALSE, uneqvar.method = "Welch")
#'
#' two.mean.t(x1bar=59.01, n1=53, s1=44.89,
#'                      x2bar=46.61, n2=54, s2=34.85,
#'                      tail="right", alpha=0.05,
#'                      equal.variance=FALSE, uneqvar.method = "Cochran")
#'
#' @export
two.mean.t <- function(x1, x2, x1bar=NULL, n1=NULL, s1=NULL,
                       x2bar=NULL, n2=NULL, s2=NULL,
                       equal.variance = TRUE, uneqvar.method ="Welch",
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
  if (is.null(s1)){s1 = sd(x1)} #sample sd
  if (is.null(s2)){s2 = sd(x2)} #sample sd

  delta <- x1bar - x2bar


  conclusion="H0 Not Rejected" #Initialized

  #pooled t test
  if (equal.variance){
    sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
    sp <- sqrt(sp2)
    se <- sp*sqrt(1/n1+1/n2)
    df=n1+n2-2
    t0 = (delta - delta0)/se

  if(tail == "left"){
    CV = qt(alpha, df=df);
    if(t0 < CV){conclusion="H0 Rejected"}
    p = pt(t0, df=df)
  }
  if(tail == "right"){
    CV = qt(1-alpha, df=df);
    if(t0 > CV){conclusion="H0 Rejected"}
    p = 1 - pt(t0, df=df)
  }
  if(tail == "two"){
    CV1 = qt(alpha/2, df=df);
    CV2 = qt(1-alpha/2, df=df);
    CV = c(CV1, CV2)
    if(t0 < CV[1] || t0 > CV[2]){conclusion="H0 Rejected"}
    p = pt(-abs(t0), df=df) + 1 - pt (abs(t0), df)
  }
  } else if (uneqvar.method =="Welch"){
    a1 = s1^2/n1; a2 = s2^2/n2
    se <- sqrt(a1 + a2)
    t0 = (delta - delta0)/se
    A = (a1 + a2)^2
    B = a1^2/(n1-1) + a2^2/(n2-1)
    Delta = A/B
    if (tail=="two"){
      q <- qt(1-alpha/2, df=Delta)
      if(t0 < -q || t0 > q){conclusion="H0 Rejected"}
      p = pt(-abs(t0), df=Delta) + 1 - pt (abs(t0), df=Delta)
    }
    if (tail == "left") {
      q <- qt(alpha, df=Delta)
      if(t0 < q){conclusion="H0 Rejected"}
      p = pt(t0, df=Delta)
    }
    if (tail == "right"){
      q <- qt(1-alpha, df=Delta)
      if(t0 > q){conclusion="H0 Rejected"}
      p = 1 - pt(t0, df=Delta)
    }
    #CI calculation
    A.CI = (a1 + a2)^2
    B.CI = a1^2/(n1-1) + a2^2/(n2-1)
    Delta.CI = A/B
    cv.CI <- qt(1-alpha/2, df=Delta)
  } else if (uneqvar.method =="Cochran") {
    a1 = s1^2/n1; a2 = s2^2/n2
    se <- sqrt(a1 + a2)
    t0 = (delta - delta0)/se
    if (tail=="two"){
      q1 <- qt(1-alpha/2, df=n1-1)
      q2 <- qt(1-alpha/2, df=n2-1)
      q <- (a1*q1 + a2*q2) / (a1+a2)
      if(t0 < -q || t0 > q){conclusion="H0 Rejected"}
    }
    if (tail == "left") {
      q1 <- qt(alpha, df=n1-1)
      q2 <- qt(alpha, df=n2-1)
      q <- (a1*q1 + a2*q2) / (a1+a2)
      if(t0 < q){conclusion="H0 Rejected"}
    }
    if (tail == "right"){
      q1 <- qt(1-alpha, df=n1-1)
      q2 <- qt(1-alpha, df=n2-1)
      q <- (a1*q1 + a2*q2) / (a1+a2)
      if(t0 > q){conclusion="H0 Rejected"}
    }
    #CI calculation
    q1.CI <- qt(1-alpha/2, df=n1-1)
    q2.CI <- qt(1-alpha/2, df=n2-1)
    cv.CI <- (a1*q1 + a2*q2) / (a1+a2)
  }
  #CI always 2-sided
  margin = cv.CI*se#margin of error

  L = delta - margin; R = delta + margin
  o = list()
  o$CI = c(L, R); o$xbar = c(x1bar, x2bar);
  o$mu_diff = delta; o$n = c(n1, n2); o$margin = margin;
  if (equal.variance){o$p=p} else if (uneqvar.method=="Welch") {o$p=p}
  o$s = c(s1, s2);
  if (equal.variance){o$sp = sp} else {o$se=se};
  o$t0 = t0;
  if (equal.variance){
    o$df = df
  } else if (uneqvar.method=="Cochran") {
    o$df = c(n1-1, n2-1)
  } else if (uneqvar.method=="Welch") {
    o$df = Delta
  }
  if (equal.variance){o$CV = CV} else {o$CV = q}
  o$conclusion = conclusion;
  o$equal.variance = equal.variance
  o$uneqvar.method	= uneqvar.method

  return(o)
}

