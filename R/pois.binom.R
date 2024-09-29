#' Experiment to use Poisson (lambda = np) to approximate Binomial(n, p)
#'
#' This function demonstrates the approximation of Binomial(n, p) by Poisson (lambda = np)
#' 
#' @param n Size of sample for binomial distribution (n, p)
#' @param p Probability of success for binomial distribution (n, p)
#' 
#' @return Probabilities of two distributions
#' 
#' @examples
#' #Binom(100, 0.3)
#' pois.binom(n = 100, p = 0.3)
#' 
#' pois.binom(n = 1000, p = 0.003)
#' 
#' @export 
#'
pois.binom <- function(n, p, x.range=NULL, out = FALSE){
  #Binomial
  x <- 0:n
  p.binom <- dbinom(x, size=n, prob=p)
  
  #Poisson (lambda = np)
  p.pois <- dpois(x, lambda=n*p)
  
  if(is.null(x.range)){
    x.range <- rep(NA, 2)
    x.range[1] <- max(0, n*p-4*sqrt(n*p))
    x.range[2] <- min(n, n*p+4*sqrt(n*p))
  }
  par(mfrow=c(1,2))
  #Plot binomial
  m <- max(p.binom, p.pois)
  bp1 <- barplot(p.binom, col="turquoise", 
         main=paste("Binomial (n=", n, ", p=", p , ")"), 
         ylim=c(0, m), xlim=x.range)
  #mtext(side = 1, at = bp1, line = 0, text = x, cex=0.8)
  
  #Plot Poisson
  bp2 <- barplot(p.pois, col="green", 
                 main=paste("Poisson(lambda=np=", n*p, ")"), 
                 ylim=c(0, m), xlim=x.range)
  #mtext(side = 1, at = bp2, line = 0, text = x, cex=0.8)
  
  if (out){
    o <- list()
    o$p.binom <- p.binom
    o$p.pois <- p.pois
    return(o)
  }
}



