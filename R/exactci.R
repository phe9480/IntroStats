#' Clopper/Pearon Exact Confidence Interval (Two-Sided)
#'
#' This function calculates the Clopper-Pearson exact CI for a binomial success
#' probability for r successes out of n trials with confidence level.
#'
#' @param r number of successes
#'
#' @param n size
#'
#' @param conflev Two-sided confidence level
#'
#'
#' @examples
#'
#'  exactci(r=4, n=20, conflev=0.95)
#'
#' @export
#'
exactci <- function(r=4, n=20, conflev=0.95){
  #Two-sided Clopper Pearson Exact CI

  #Example:
  #> exactci (r=4, n=20, conflev=0.95)
  #[1] 0.057334 0.436614

  alpha = (1 - conflev)
  if (r == 0) {
    ll = 0
    ul = 1 - (alpha/2)^(1/n)
  }
  else if (r == n) {
    ll = (alpha/2)^(1/n)
    ul = 1
  }
  else {
    ll = 1/(1 + (n - r + 1) / (r * qf(alpha/2, 2 * r, 2 * (n-r+1))))
    ul = 1/(1 + (n - r) / ((r + 1) * qf(1-alpha/2, 2 * (r+1), 2 *(n-r))))
  }
  c(ll,ul)
}

