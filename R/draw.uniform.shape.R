#' Draw Uniform Shape Distribution
#'
#' This function draws a uniform Shape distribution for illustration of statistical distributions.
#'
#' @param range Range
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#'
#' #Example.
#' draw.uniform.shape(range = c(0, 1), main="Uniform Distribution")
#'
#' @export
#'
draw.uniform.shape = function(range = c(0, 1), n=1000, ...){
  x = runif(n, min=range[1], max=range[2])
  hist(x, col="turquoise", prob=TRUE, breaks = 30, ...)
  lines(density(x, width=3), col = "blue", lwd = 2)
}





