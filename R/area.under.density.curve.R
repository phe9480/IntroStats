#' Area under density curve
#'
#' This function calculates the area under a density curve. 
#' When used for discrete random variable, the area is calculated as with 
#' inclusive boundaries based on CDF.
#' 
#' @param cdf Cumulative distribution function
#' @param interval Range of the variable. For discrete random variable, 
#' interval = c(L,U) means an inclusive range of values for L+1, L+2, ..., U.
#' The calculate is P(X <= U) - P(X<=L).
#' @param ... Additional parameters passed to cdf function.
#'
#' @examples
#' Example
#' #P(-1.96 < Z < 1.96)
#' area.under.density.curve(cdf = pnorm, interval = c(-1.96, 1.96))
#' 
#' #P(0 < X < 0.5) for a CDF of x^2/4.
#' area.under.density.curve(cdf = function(x){x^2/4}, interval = c(0, 0.5))
#' 
#' #P(3 < X < 10) for X ~ binomial(15, p=0.3)
#' area.under.density.curve(cdf = pbinom, interval = c(3, 9), size=15, prob=0.3)
#' 
#' 
#' #P(3 <= X <= 10) for X ~ binomial(15, p=0.3)
#' area.under.density.curve(cdf = pbinom, interval = c(2, 10), size=15, prob=0.3)
#' 
#' #P(3 < X <= 10) for X ~ binomial(15, p=0.3)
#' area.under.density.curve(cdf = pbinom, interval = c(3, 10), size=15, prob=0.3)
#' 
#' #P(3 <= X < 10) for X ~ binomial(15, p=0.3)
#' area.under.density.curve(cdf = pbinom, interval = c(2, 9), size=15, prob=0.3)
#' 
#' 
#' @export
#'
area.under.density.curve=function(cdf=pnorm, interval = c(0, 0.5), ...){
  p = cdf(interval[2], ...) - cdf(interval[1], ...)
  return(p)
}
