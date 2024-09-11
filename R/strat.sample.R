#' Stratified sampling with proportional allocation
#'
#' This function sample data with proportional allocation.
#'
#' @param x an array that represents the entire population
#' @param strata stratum indicator
#' @param size sample size
#' @param proportional TRUE or FALSE. If true, the sample is proportional to the strata size.
#'
#' @examples
#' #100 numbers from 1 to 100
#' #1st stratum size 70 and 2nd stratum size 30
#' #pick a sample of size 10: 7 from 1st stratum and 3 from 2nd stratum which is proportional.
#' strat.sample(x=1:100, strata=c(rep(1, 70), rep(2, 30)), size=10, proportional = TRUE)
#'
#'
#' @export
#'
strat.sample = function(x, strata, size, proportional = TRUE){
  n = length(x)
  u.strata = unique(strata)
  n.strata = length(u.strata)
  m = rep(NA, n.strata) #size of each stratum

  for (i in 1:n.strata){m[i] = length(strata[strata == u.strata[i]])}
  ni = round(size*m/n)
  if (sum(ni) > size) {ni[n.strata] = ni[n.strata] - (sum(ni) - size)} else if (sum(ni) < size) {ni[n.strata] = ni[n.strata] + (size-sum(ni))}

  if(proportional){
    s = NULL
    for (i in 1:n.strata){
      s = c(s, sample(x[strata == u.strata[i]], size=ni[i], replace=FALSE))
    }
  } else {
    s= sample(x, size=size, replace=FALSE)
  }
  return(s)
}


