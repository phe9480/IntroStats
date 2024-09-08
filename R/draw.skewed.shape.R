#' Draw Skewed Distribution
#'
#' This function draws a skewed distribution for illustration of statistical distributions.
#'
#' @param peaks.loc Peak location
#' @param width Spread of each peak
#' @param skew Optionsl include "right", "left", "no"
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#'
#' #Example.
#' draw.skewed.shape(peak.loc=10, width=1, skew = "right",
#' main="Right Skewed Distribution",
#' xlim=c(0, 50), ylim=c(0, 0.22))
#'
#' draw.skewed.shape(peak.loc=10, width=2, skew = "right",
#' main="Right Skewed Distribution",
#' xlim=c(0, 50), ylim=c(0, 0.22))
#'
#' draw.skewed.shape(peak.loc=10, width=2, skew = "left",
#' main="Left Skewed Distribution",
#' xlim=c(-20, 20), ylim=c(0, 0.17))
#'
#' draw.skewed.shape(peak.loc=10, width=3, skew = "left",
#' main="Left Skewed Distribution",
#' xlim=c(-20, 20), ylim=c(0, 0.17))
#'
#' draw.skewed.shape(peak.loc=10, width=2, skew = "no",
#' main="Not Skewed (symmetric) Distribution",
#' xlim=c(0, 20), ylim=c(0, 0.22))
#'
#' draw.skewed.shape(peak.loc=10, width=3, skew = "no",
#' main="Not Skew (symmetric) Distribution",
#' xlim=c(0, 20), ylim=c(0, 0.22))
#'
#' @export
#'
draw.skewed.shape = function(peak.loc=10, width=1, skew = "right", n=1000, ...){
  x1 = rnorm(n, mean=peak.loc, sd=width)
  if (skew == "right"){
    x2 = rexp(n, rate=log(2)/4) + peak.loc
  } else if (skew == "left"){
    x2 = -rexp(n, rate=log(2)/4) + peak.loc
  } else {
    x2 = NULL
  }
  x=c(x1, x2)
  hist(x, col="turquoise", prob=TRUE,  breaks=30, ...)
  lines(density(x), col = "blue", lwd = 2)
}

