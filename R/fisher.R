#' Fisher's Exact Test
#'
#' This function calculates the Fisher's Exact test. 
#' It is equivalent to fisher.test, however provides the convenience of direct 
#' calculation based on number of successes and number of trials in each group.
#' 
#' @param r1 Number of successes in group 1
#' @param n1 Number of trials in group 1
#' @param r2 Number of successes in group 2
#' @param n2 Number of trials in group 2
#' @param alternative Type of Ha: "two.sided", "less", "greater"
#' @param conflev Level of confidence interval
#' @param ... Additional parameters to pass to fisher.test(). Check ?fisher.test for details.
#' 
#' @return An object that has components
#' \describe{
#' \item{p}{P value}
#' \item{odds.ratio.ci}{Confidence interval for odds ratio}
#' \item{odds.ratio}{Odds Ratio for group 1 vs group 2}
#' \item{Ha}{Type of hypothesis test: two.sided, less, or greater}
#' \item{data}{2 by 2 contingency table}
#' \item{phat}{Estimates of proportions in two groups}
#' \item{p1.exact.ci}{Exact CI by Clopper-Pearson method for p1}
#' \item{p2.exact.ci}{Exact CI by Clopper-Pearson method for p2}
#' }
#' 
#' @examples
#' #Example: 107 responses out of 245 subjects vs 52 responses out of 232 subjects
#' fisher(r1=107, n1=245, r2=52, n2=232)
#' 
#' fisher(r1=6, n1=245, r2=38, n2=232)
#' 
#' @export 
#'
fisher <- function(r1, n1, r2, n2, alternative = "two.sided",
                   conflev = 0.95,...){
  M <- as.table(rbind(c(r1,r2), c(n1-r1, n2-r2)))
  dimnames(M) <- list(Response = c("Y", "N"),
                      Population = c("Group 1","Group 2"))
   
  f <- fisher.test(M, alternative = alternative,
              conf.int = TRUE, conf.level = conflev, ...)
  o <- list()
  o$p = f$p.value
  o$odds.ratio.ci = f$conf.int
  o$odds.ratio = f$estimate
  o$Ha = f$alternative
  o$data = M
  o$phat = c(r1, r2)/c(n1, n2)
  o$p1.exact.ci = ci.p(r=r1, n=n1, conflev=conflev, method="Clopper-Pearson Exact")
  o$p2.exact.ci = ci.p(r=r2, n=n2, conflev=conflev, method="Clopper-Pearson Exact")
  return(o)
} 



