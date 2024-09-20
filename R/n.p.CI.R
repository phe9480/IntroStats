#' Sample Size calculation for achieving adequate precision in population proportion estimate
#'
#' This function calculates the sample size required to achieve adequate precision in population proportion estimate
#'
#' @param E Precision of proportion estimate, i.e. half of CI length
#' @param g A range of likely population proportion
#' @param conflev Confidence level, eg, 0.95
#'
#' @examples
#' Example
#' n.p.CI(E=0.1, conflev=0.95, g=c(0.1, 0.3))
#'
#' @export
n.p.CI <- function(E=0.02, conflev=0.95, g=c(0.4, 0.7)){
  #g: range of possible p
  if(length(g)==1){pg=g} else{
    if(prod(g-0.5)<=0){
      pg=0.5
    }else if(abs(g[1]-0.5)< abs(g[2]-0.5)){
      pg=g[1]
    } else{pg=g[2]}
  }
  z = qnorm(1-(1-conflev)/2); n = ceiling(pg*(1-pg)*(z/E)^2)
  o=list(); o$pg=pg; o$n=n
  return(o)
}
