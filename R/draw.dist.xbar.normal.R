#' Illustration of sampling distribution of x_bar with a sample drawn from a normal distribution
#'
#' This function draws the density curves of x and x_bar.
#'
#' @param n Sample size
#' @param mu Mean of a population
#' @param sigma Standard deviation from a population
#' @param ... Further graphical parameters
#' @examples
#' #Example.
#' draw.dist.xbar.normal(n=10, sigma=2, mu=10)
#'
#' @export
#'
draw.dist.xbar.normal <- function(n=100, sigma=2, mu=10, ...){
  x = seq(mu-3*sigma, mu+3*sigma, by=0.01)
  y = dnorm(x, mean = mu, sd=sigma)

  y1 = dnorm(x, mean = mu, sd=sigma/sqrt(n))

  plot(x, y, type="n", ylim=c(0, max(c(y, y1))),xlab="x", ylab="", ...)
  lines(x, y, col=1, lty=1, lwd=2)
  lines(x, y1, col="turquoise", lty=1, lwd=2)
  legend("topright", bty="n", cex=0.8, c("Distribution of X", "Distribution of X_bar"),
         col=c("black", "turquoise"), lwd=rep(2, 2))

}


