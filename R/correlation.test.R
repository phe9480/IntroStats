#' Perform Hypothesis Test for Linear Correlation
#'
#' This functions performs the hypothesis test for linear correlation between x and y.
#'
#' @param x predictor variable
#' @param y response variable
#' @param alpha Significance level
#' @param tail Side of hypothesis testing: "two", "left", "right". Default "two"
#' 
#' @return Linear correlation, CI of linear correlation, 
#' cv for testing the linear correlation, observed value of t-test statistic, 
#' p value.
#' 
#' @examples
#' 
#' x = rnorm(100, mean=40, sd=10)
#' y = rnorm(100, mean=20, sd=20)
#' correlation.test (x, y, alpha=0.05, tail="two")
#' 
#' @export 
#' 
correlation.test = function(x, y, alpha=0.05, tail="two"){
  lr = OLR(x, y)
  r = sqrt(lr$r2)
  n = length(x)
  t0 = r / sqrt((1-r^2)/(n-2))
  
  if (tail == "two") {
    q <- qt(1 - alpha/2, df = n-2)
    if (t0 < -q || t0 > q) {conclusion = "H0 Rejected"}
    p = 2*pt(-abs(t0), df = n-2)
  }
  if (tail == "left") {
    q <- qt(alpha, df = n-2)
    if (t0 < q) {conclusion = "H0 Rejected"}
    p = pt(t0, df = n-2)
  }
  if (tail == "right") {
    q <- qt(1 - alpha, df = n-2)
    if (t0 > q) {conclusion = "H0 Rejected"}
    p = 1 - pt(t0, df = n-2)
  }
  
  margin = q * sqrt((1-r^2)/(n-2))
  L = r - margin
  R = r + margin
  o = list()
  o$r = r
  o$CI = c(L, R)
  o$cv = q
  o$t0 = t0
  o$p = p
  
  return(o)
}
