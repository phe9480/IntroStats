#' Draw Bell-Shaped Distribution
#'
#' This function draws a Bell-Shaped distribution for illustration of statistical distributions.
#'
#' @param peaks.loc Peak location
#' @param width Spread of each peak
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#' #Example.
#' draw.bell.shape(peak.loc=10, width=2, main="Bell Shaped Distribution")
#'
#' @export
#'
draw.bell.shape = function(peak.loc = 0, width=1, n=1000, ...){
  x = rnorm(n, mean=peak.loc, sd=width)
  hist(x, col="turquoise", prob=TRUE, breaks = 30, ...)
  lines(density(x), col = "blue", lwd = 2)

}


