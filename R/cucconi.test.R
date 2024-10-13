#' Cucconi Test comparing two distribution functions
#'
#' This function performs Cucconi Test comparing two distribution functions equality. 
#' Implementation of the Cucconi test for the two-sample location-scale problem. 
#' A permutation/bootstrap distribution of the test statistic (C) under the 
#' null hypothesis is used to calculate the p-value. 
#' Reference: Marozzi (2013), p. 1302-1303
#' 
#' @param x sample 1
#' @param y sample 2
#' @param method "permutation" or "bootstrap" method to construct the empirical distributions
#' 
#' @return p value, test statistic (C)
#' 
#' @examples
#' #Example
#' 
#' x <- rnorm(30)
#' y <- rnorm(30) * 1.3 + 0.5
#' cucconi.test(x = x, y = y)
#' cucconi.test(x = x, y = y, method="bootstrap")
#' 
#' @export 
#'
cucconi.test <- function(x, y, method = c("permutation", "bootstrap")){
  
  #1, Calculates the test statistic for the Cucconi two-sample location-scale test
  cucconi.teststat <- function(x, y, m = length(x), n = length(y)){
    N <- m + n
    S <- rank(c(x, y))[(m + 1):N]
    denom <- sqrt(m * n * (N + 1) * (2 * N + 1) * (8 * N + 11) / 5)
    U <- (6 * sum(S^2) - n * (N + 1) * (2 * N + 1)) / denom
    V <- (6 * sum((N + 1 - S)^2) - n * (N + 1) * (2 * N + 1)) / denom
    rho <- (2 * (N^2 - 4)) / ((2 * N + 1) * (8 * N + 11)) - 1
    C <- (U^2 + V^2 - 2 * rho * U * V) / (2 * (1 - rho^2))
    return(C)
  }
  #2. distribution by permutation
  cucconi.dist.perm <- function(x, y, reps = 100000){
    
    # Computes the distribution of the Cucconi test statistic 
    #using random permutations
    
    m <- length(x)
    n <- length(y)
    N <- m + n
    alldata <- c(x, y)
    
    bootFunc <- function(){
      permdata <- alldata[sample(1:N, size = N, replace = FALSE)]
      xperm <- permdata[1:m]
      yperm <- permdata[(m + 1):N]
      return(cucconi.teststat(x = xperm, y = yperm, m = m, n = n))    
    }
    permvals <- replicate(reps, expr = bootFunc())
    
    #  permvals<-rep(NA,times=reps)
    #  for(r in 1:reps){
    #    permdata<-alldata[sample(1:N,size=N,replace=FALSE)]
    #    xperm<-permdata[1:m]
    #    yperm<-permdata[(m+1):N]
    #    permvals[r]<-cucconi.teststat(x=xperm,y=yperm, m=m, n=n)
    #  }
    return(permvals)
  }
  #3. distribution by bootstrapping
  cucconi.dist.boot <- function(x, y, reps = 100000){
    
    # Computes the distribution of the Cucconi test statistic 
    # using bootstrap sampling
    
    m <- length(x)
    n <- length(y)
    x.s <- (x - mean(x)) / sd(x) # standardise the x-values
    y.s <- (y - mean(y)) / sd(y) # standardise the y-values
    
    bootFunc <- function(){
      xboot <- x.s[sample(1:m, size = m, replace = TRUE)]
      yboot <- y.s[sample(1:n, size = n, replace = TRUE)]
      return(cucconi.teststat(x = xboot, y = yboot, m = m, n = n))
    }
    bootvals <- replicate(reps, expr = bootFunc())
    
    #  bootvals <- rep(NA,times=reps)
    #  for(r in 1:reps){
    #    xboot<-x.s[sample(1:m,size=m,replace=TRUE)]
    #    yboot<-y.s[sample(1:n,size=n,replace=TRUE)]
    #    bootvals[r]<-cucconi.teststat(x=xboot,y=yboot, m=m, n=n)
    #  }
    return(bootvals)
  }
  
    m <- length(x)
    n <- length(y)
    C <- cucconi.teststat(x = x, y = y, m = m, n = n)
    
    if(method[1] == "permutation"){
      h0dist <- cucconi.dist.perm(x = x, y = y)
    }
    
    if(method[1] == "bootstrap"){
      h0dist <- cucconi.dist.boot(x = x, y = y)
    }
    
    p.value <- length(h0dist[h0dist >= C]) / length(h0dist)
    
    cat("\nCucconi two-sample location-scale test\n")
    cat("\nNull hypothesis: The locations and scales of the 
				two population distributions are equal.\n")
    cat("Alternative hypothesis: The locations and/or scales of 
				the two population distributions differ.\n")
    cat(paste("\nC = ", round(C, 3), ", p-value = ", 
              round(p.value, 4), "\n\n", sep=""))
    
    return(list(C = C, method = method[1],
                p.value = p.value))
}
  
  
  
  




