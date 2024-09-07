#' Confidence Interval (Two-Sided) of mean with known or unknown variance
#'
#' This function calculates the CI of mean with known or unknown variance assuming normal distribution.
#'
#' @param xbar Sample mean
#' @param n Sample size
#' @param x Sample. Default NULL. When x is available, then use x to calculate xbar, n, and s.
#' @param sd Known standard deviation. Either x or sd is required when method = "Known Variance". This is the population sd
#' @param s sample standard deviation Either x or s is required when method = "Unknown Variance"
#' @param conflev Two-sided confidence level
#'
#'
#' @examples
#' #Example (1). The sample x is provided.
#' x = rnorm(100, mean=10, sd=2)
#'
#' ci.mu(x=x, conflev=0.95, sd = 2, method="Known Variance")
#'
#' ci.mu(x=x, conflev=0.95, method="Unknown Variance")
#'
#' #Example (2). Sample mean and n are provided.
#'
#' xbar = mean(x); s = sd(x); n = 100
#'
#' ci.mu(xbar=xbar, sd = 2, n = 100, conflev=0.95, method="Known Variance")
#'
#' ci.mu(xbar=xbar, s=s, n = 100, conflev=0.95, method="Unknown Variance")
#'
#' @export
#'
ci.mu <- function(xbar, n, x=NULL, s=NULL, sd=1, conflev=0.95, method="Known Variance"){
  alpha <- 1-conflev
  if(!is.null(x)){xbar = mean(x); s=sd(x); n=length(x)}

  if (method == "Known Variance"){
    q <- qnorm(1-alpha/2)
    se <- sd/sqrt(n)
    L <- xbar - q*se
    U <- xbar + q*se
  } else if (method == "Unknown Variance") {
    #t-distribution
    q <- qt(1-alpha/2, df=n-1)
    se <- s/sqrt(n)
    L <- xbar - q*se
    U <- xbar + q*se
  }
  return(c(L, U))
}
