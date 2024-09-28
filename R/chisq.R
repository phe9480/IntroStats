#' Chi-square Test
#'
#' This function calculates the Chi-square test. Because the test statistic is z value squared, 
#' it is always one-sided, however it is equivalent to two-sided test in Z test.
#' It is equivalent to function chisq.test(), however provides the convenience of direct 
#' calculation based on number of successes and number of trials in each group.
#' 
#' @param r1 Number of successes in group 1
#' @param n1 Number of trials in group 1
#' @param r2 Number of successes in group 2
#' @param n2 Number of trials in group 2
#' @param correct Continuity correction, TRUE or FALSE
#' @param conflev Level of confidence interval
#' @param ... Additional parameters to pass to chisq.test(). Check ?chisq.test for details.
#' 
#' @return An object that has components
#' \describe{
#' \item{p}{P value}
#' \item{chisq}{Value of test statistic chi-square}
#' \item{df}{Degrees of freedom for chi-square test}
#' \item{method}{Test method}
#' \item{observed}{2 by 2 table for observed values}
#' \item{expected}{2 by 2 table for expected values}
#' \item{residuals}{2 by 2 table for Pearson residuals, (observed - expected) / sqrt(expected)}
#' \item{$standardized.residuals}{2 by 2 table for standardized residuals,
#' (observed - expected) / sqrt(V), where V is the residual cell variance 
#' (Agresti, 2007, section 2.4.5 for the case where x is a matrix, np(1 - p) otherwise).}
#' \item{phat}{Estimates of proportions in two groups}
#' \item{p1.exact.ci}{Exact CI by Clopper-Pearson method for p1}
#' \item{p2.exact.ci}{Exact CI by Clopper-Pearson method for p2}
#' \item{p1.ci}{CI for p1 by asymptotic normal method with or without continuity correction according to parameter correct}
#' \item{p2.ci}{CI for p2 by asymptotic normal method with or without continuity correction according to parameter correct}
#' }
#' 
#' @examples
#' #Example: 107 responses out of 245 subjects vs 52 responses out of 232 subjects
#' chisq(r1=107, n1=245, r2=52, n2=232, correct=TRUE, conflev = 0.95)
#' 
#' chisq(r1=6, n1=245, r2=38, n2=232, correct=TRUE, conflev = 0.95)
#' 
#' @export 
#'
chisq <- function(r1, n1, r2, n2, correct=TRUE, conflev = 0.95,...){
  M <- as.table(rbind(c(r1,r2), c(n1-r1, n2-r2)))
  dimnames(M) <- list(Response = c("Y", "N"),
                      Population = c("Group 1","Group 2"))
   
  f <- chisq.test(M, correct=correct, ...)
  o <- list()
  o$chisq = f$statistic
  o$df = f$parameter
  o$p = f$p.value
  
  o$observed = f$observed
  o$expected = f$expected
  o$residuals = f$residuals
  o$standardized.residuals = f$stdres
  
  o$phat = c(r1, r2)/c(n1, n2)
  o$p1.exact.ci = ci.p(r=r1, n=n1, conflev=conflev, method="Clopper-Pearson Exact")
  o$p2.exact.ci = ci.p(r=r2, n=n2, conflev=conflev, method="Clopper-Pearson Exact")
  if (correct){
    o$p1.ci = ci.p(r=r1, n=n1, method="Asymptotic Normal with Continuity", conflev=conflev)
    o$p2.ci = ci.p(r=r2, n=n2, method="Asymptotic Normal with Continuity", conflev=conflev)
  } else {
    o$p1.ci = ci.p(r=r1, n=n1, method="Asymptotic Normal", conflev=conflev)
    o$p2.ci = ci.p(r=r2, n=n2, method="Asymptotic Normal", conflev=conflev)
  }
  o$method = f$method
  return(o)
} 



