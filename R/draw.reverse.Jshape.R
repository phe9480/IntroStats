#' Draw Reverse-J Shaped Distribution
#'
#' This function draws a reverse-J shaped distribution for illustration of statistical distributions.
#'
#' @param median.loc Location of median
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#' #Example.
#' draw.reverse.Jshape (median.loc=10)
#'
#' @export
#'
draw.reverse.Jshape = function(median.loc = 10, n = 1000, ...){
  x = rexp(n, rate = log(2)/median.loc)

  hist(x, col="turquoise", prob=TRUE,  breaks=30, ...)

  xx = seq(0, max(x), by=1)
  yy = dexp(xx, rate=log(2)/median.loc)
  lines(xx, yy, col="blue", lwd=2)

}


