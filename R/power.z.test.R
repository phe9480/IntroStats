#' Sample size calculation for One sample z test
#'
#' This function calculates the sample size based on one sample z test
#'
#' @param power Power
#' @param delta Mean difference: mu - mu0
#' @param sd Population sd
#' @param alternative "one.sided"
#' @param sig.level significance level, alpha
#' @param type "one.sample"
#'
#'
#' @examples
#' #Example.
#' power.z.test(power = 0.9, delta = 1, sd=1, alternative = "one.sided", 
#' sig.level=0.05, type="one.sample")
#'
#' @export
power.z.test = function(power = 0.9, delta = 1, sd=1, alternative = "one.sided", 
                        sig.level=0.05, type="one.sample"){
  alpha <- sig.level
  if (alternative == "one.sided"){
    za = qnorm(1-alpha); zb = qnorm(power)
  } else {
    za = qnorm(1-alpha/2); zb = qnorm(power)
  }
  if (type == "one.sample"){
    n = (za+zb)^2*sd^2/delta^2
  }
  return(n)
}
