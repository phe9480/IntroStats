#' Plot F distribution density to illustrate the features of F distribution
#'
#' This function plots the F distribution density to illustrate the features.
#' 
#' @param mu mean parameter in normal distribution
#' @param sigma sd parameter in normal distribution
#' @param ... additional graphic parameters
#' 
#' @return graph
#' 
#' @examples
#' #N(10, sd=3)
#' draw.normal.dist(mu=10, sigma=3, xlim=c(-20, 40), ylim=c(0,0.13))
#' 
#' @export 
#'
draw.normal.dist <- function(mu, sigma, ...){
  x <- seq(mu-3*sigma, mu+3*sigma, length.out=100)
  plot(x, dnorm(x, mean=mu, sd=sigma), type="l", lwd=3, col="turquoise",
       main=paste("N(", mu, ", ", sigma, ") Density Function", sep = ""),
       xlab="", ylab="Density N(mu, sd)",...)
}




