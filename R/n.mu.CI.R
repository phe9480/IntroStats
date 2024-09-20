#' Sample Size calculation for achieving adequate precision in population mean estimate
#'
#' This function calculates the sample size required to achieve adequate precision in population mean estimate
#'
#' @param E Precision of mean estimate, i.e. half of CI length
#' @param sigma Population standard deviation
#' @param conflev Confidence level, eg, 0.95
#'
#' @examples
#' Example
#' n.mu.CI(E=0.5, sigma=1, conflev=0.99)
#'
#' @export
#'
n.mu.CI <- function(E, sigma, conflev=0.95){
  alpha <- 1- conflev
  z <- qnorm(1-alpha/2)
  n <- ceiling(z^2*sigma^2/E^2)
  return(n)
}
