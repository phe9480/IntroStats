#' Plot Shade Under a Density Curve
#'
#' This function plots the F distribution density to illustrate the features.
#' 
#' @param dfun density function
#' @param range range of x
#' @param shade.interval interval for drawing shade under the curve
#' @param curve.col color of the density curve
#' @param shade.col color of the shade
#' @param xlab  x-axis label
#' @param ylab y-axis label description
#' @param main title of the graph#' 
#' @param ... additional parameters pass to dfun() function
#' 
#' @return graph
#' 
#' @examples
#' #N(0, 1) with shade between (-1, 1)
#' draw.shade.under.density(dfun = dnorm, range=c(-3, 3), 
#' shade.interval=c(-1, 1), curve.col = "aquamarine4", shade.col = "turquoise",
#' xlab="x", ylab="Density Curve of N(0, 1)", mean=0, sd=1)
#' 
#' @export 
#'
draw.shade.under.density <- function(dfun = dnorm, range=c(-3, 3), 
                                     shade.interval=c(-1, 1), 
                                     curve.col = "aquamarine4",
                                     shade.col = "turquoise",
                                     xlab="x", ylab="y", main="Density of N(0, 1)", ...){
  x <- seq(range[1], range[2], length.out=500)
  d <- dfun(x, ...)
  plot(x, d, type="n", xlab=xlab, ylab=ylab, main=main)
  select <- (x>=shade.interval[1] & x <= shade.interval[2])
  new.x <- c(shade.interval[1], x[select], shade.interval[2])
  new.y <- c(0, d[select], 0)
  polygon(new.x, new.y, border = FALSE, col=shade.col)
  lines(x, d, col=curve.col, lwd=3)
}




