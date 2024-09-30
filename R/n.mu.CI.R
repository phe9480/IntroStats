#' Sample Size calculation for achieving adequate precision in population mean estimate
#'
#' This function calculates the sample size required to achieve adequate precision in population mean estimate
#'
#' @param E Precision of mean estimate, i.e. half of CI length
#' @param sigma Population standard deviation
#' @param conflev Confidence level, eg, 0.95
#' @param method "Z" for Z interval or "t" for t interval
#'
#' @examples
#' Example
#' n.mu.CI(E=0.6, sigma=1, conflev=0.99)
#' 
#' n.mu.CI(E=0.6, sigma=1, conflev=0.99, method="t")
#'
#' @export
#'
n.mu.CI <- function(E, sigma, conflev=0.95, method="Z"){
  alpha <- 1- conflev
  z <- qnorm(1-alpha/2)
  n <- max(ceiling(z^2*sigma^2/E^2), 2) #n at least 2
  
  if (method == "Z") {
    return(n)
  } else if (method =="t"){
    q <- qt(1-alpha/2, df=n-1)
    while (q*sigma/sqrt(n) > E) {
      n <- n + 1
      q <- qt(1-alpha/2, df=n-1)
    }
    nt <- ceiling(q^2*sigma^2/E^2)
    return(nt)
  }
}


