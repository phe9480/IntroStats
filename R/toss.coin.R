#' Simulation of tossing a coin
#'
#' This function performs the simulation of tossing balanced coins. 
#' 
#' @param n Number of tosses
#' 
#' @return A data set of outcomes
#' 
#' @examples
#' #Toss 5 times
#' toss.coin(n=5)
#' 
#' @export 
#'
toss.coin <- function(n){
  H <- rbinom(1, size=n, prob=0.5)
  
  outcome <- sample(c(rep("H", H), rep("T", n-H)))
  o=list()
  o$heads = H; o$outcome=outcome
  return(o)
} 



