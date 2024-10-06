#' Plot Shade Under a Density Curve
#'
#' This function plots the F distribution density to illustrate the features.
#' 
#' @param y sample data of a continuous variable
#' @param ... additional graphical parameters pass to plot
#' 
#' @return A dataframe with normal scores (expected) and original sorted sample
#' 
#' @examples
#' y <- rnorm(100, mean=10, sd=3)
#' normal.prob.plot(y, col="turquoise")
#' 
#' @export 
#'
normal.prob.plot <- function(y, main="Normal Probability Plot", ...) {

  qq <- qqnorm(sort(y))
  
  y = qq$y
  normal.score = qq$x
  
  data <- data.frame(y, normal.score)
  
  plot(y, normal.score, main=main, ...)
  
  return(data)  
} 




