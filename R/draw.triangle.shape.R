#' Draw Triangular Shape Distribution
#'
#' This function draws a Triangular Shape distribution for illustration of statistical distributions.
#'
#' @param peaks.loc Peak location
#' @param slope Slope of peak
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#'
#' #Example.
#' draw.triangle.shape(peak.loc=10, slope=0.25, n=20,
#' main="Triangle Shaped Distribution",
#' ylim=c(0, 0.5), xlim=c(7, 13))
#'
#' draw.triangle.shape(peak.loc=10, slope=1, n=20,
#' main="Triangle Shaped Distribution",
#' ylim=c(0, 0.5), xlim=c(7, 13))
#'
#' draw.triangle.shape(peak.loc=10, slope=4, n=20,
#' main="Triangle Shaped Distribution",
#' ylim=c(0, 0.5), xlim=c(7, 13))
#'
#' @export
#'
draw.triangle.shape = function(peak.loc = 0, slope = 1, n=10, ...){
  bins = 25
  m = median(1:bins)

  x = NULL
  h = rep(0, bins) #number of data points at location i
  for (i in 1:bins){

    if (i <= m) {
      h[i] = slope * i
    } else {
      h[i] = 2*m*slope-slope*i
    }

    x = c(x, rnorm(round(n*h[i]), mean=peak.loc, sd=1))
  }

  hist(x, col="turquoise", prob=TRUE, breaks = 30, ...)
}



