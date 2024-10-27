#' Bootstrap linear regression - Residual method
#'
#' This function performs Bootstrap linear regression - Residual method. Compared to the empirical method, this method is more robust.
#'
#'  
#' @param x covariate
#' @param y response
#' @param conflev confidence level for producing the CI of beta1. Default 0.95.
#' @param B number of bootstrapping replicates
#'
#' @return An object with values including
#'  \itemize{
#'  \item  beta: coefficients
#'  \item  beta0.CI: CI of beta0
#'  \item  beta1.CI: CI of beta1
#'  \item  se.beta: se of beta0 and beta1
#'  }
#'
#' @examples
#' #Example
#' set.seed(2023)
#' x <- rnorm(50, mean=10, sd=2)
#' y <- rnorm(50, mean=2*x+3, sd=2)
#' residual.boot.lm(x, y)
#'
#' @export
residual.boot.lm <- function(x, y, conflev=0.95, seed=2023, B=10000){
  set.seed(seed)
  
  beta <- matrix(NA, ncol=2, nrow=B)
  lm0 <- lm(y ~ x)
  e <- resid(lm(y ~ x))
  X <- matrix(c(rep(1, length(x)), x), byrow = FALSE, ncol = 2)
  beta.hat <- matrix(coef(lm0), byrow=TRUE, ncol=1)
  
  for (b in 1:B){
    eb <- sample(e, replace=TRUE)
    yb <- X%*%beta.hat + eb
    
    lm.fit <- lm(yb ~ x)
    beta[b,] <- coef(lm.fit)
  }
  beta0.hat <- mean(beta[,1])
  beta1.hat <- mean(beta[,2])
  
  #se
  se.beta0.hat <- sd(beta[,1])
  se.beta1.hat <- sd(beta[,2])
  
  #beta0    
  L0 <- quantile(beta[,1], (1-conflev)/2)
  R0 <- quantile(beta[,1], 1-(1-conflev)/2)
  
  #beta1    
  L1 <- quantile(beta[,2], (1-conflev)/2)
  R1 <- quantile(beta[,2], 1-(1-conflev)/2)
  
  
  o<-list()
  o$beta.hat = c(beta0.hat, beta1.hat)
  o$beta0.CI = c(L0, R0)
  o$beta1.CI = c(L1, R1)
  o$se.beta = c(se.beta0.hat, se.beta1.hat)
  
  return(o)
}