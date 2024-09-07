#' Confidence Interval (Two-Sided) Calculation
#'
#' This function implements multiple methods for CI calculation for a binomial success
#' probability for r successes out of n trials with confidence level.
#'
#' @param r number of successes
#'
#' @param n size
#'
#' @param conflev Two-sided confidence level
#' @param method Confidence interval method. Options include "Clopper-Pearson Exact",
#' "Wilson Score", "Wilson Score with Continuity", "Asymptotic Normal", and "Asymptotic Normal with Continuity".
#'
#' @examples
#'
#' ci(r=4, n=20, conflev=0.95, method="Clopper-Pearson Exact")
#' ci(r=4, n=20, conflev=0.95, method="Wilson Score")
#' ci(r=4, n=20, conflev=0.95, method="Wilson Score with Continuity")
#' ci(r=4, n=20, conflev=0.95, method="Asymptotic Normal")
#' ci(r=4, n=20, conflev=0.95, method="Asymptotic Normal with Continuity")
#'
#' @export
#'
ci = function(r, n, conflev, method="Clopper-Pearson Exact"){
  #1.
  exactci <- function(r, n, conflev){
    #Two-sided Clopper Pearson Exact CI
    #r: number of successes
    #n: total number of trials in binomial distribution
    #Example: exactci (r=4, n=20, conflev=0.95)
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
  #############################
  #2. Score (Wilson) CI
  #############################
  wilsonci <- function(r, n, conflev){

    alpha = (1 - conflev)
    phat = r / n
    za = qnorm(1-alpha/2)
    A = 2 * n * phat + za^2
    B = za * sqrt(za^2 + 4*n*phat*(1-phat))
    C = 2 * (n + za^2)
    ll = (A-B) / C
    ul = (A+B) / C
    c(ll,ul)
  }

  #############################
  #3. Score with continuity CI
  #############################
  score_contci <- function(r, n, conflev){

    alpha = (1 - conflev)
    phat = r / n
    za = qnorm(1-alpha/2)
    A1 = 2 * n * phat + za^2 - 1
    B1 = za * sqrt(za^2 - (2+1/n) + 4*phat*(n*(1-phat)+1))
    A2 = 2 * n * phat + za^2 + 1
    B2 = za * sqrt(za^2 + (2-1/n) + 4*phat*(n*(1-phat)-1))
    C = 2 * (n + za^2)
    ll = (A1-B1) / C
    ul = (A2+B2) / C
    c(ll,ul)
  }
  #############################
  #4. Simple Asymptotic
  #############################
  simpleci <- function(r, n, conflev){
    #Two-sided Score with continuity CI
    #r: number of successes
    #n: total number of trials in binomial distribution
    #Example: Simpleci (r=4, n=20, conflev=0.95)
    #> Simpleci (r=4, n=20, conflev=0.95)
    #[1] 0.02469549 0.37530451

    alpha = (1 - conflev)
    phat = r / n
    za = qnorm(1-alpha/2)
    B = za * sqrt(phat*(1-phat)/n)

    ll = phat - B
    ul = phat + B
    c(ll,ul)
  }
  #############################
  #5. Simple Asymptotic with continuity correction
  #############################
  simple_contci <- function(r, n, conflev){
    #Two-sided Score with continuity CI
    #r: number of successes
    #n: total number of trials in binomial distribution
    #Example: simple_contci (r=4, n=20, conflev=0.95)
    #> simple_contci (r=4, n=20, conflev=0.95)
    #[1] -0.0003045081  0.4003045081

    alpha = (1 - conflev)
    phat = r / n
    za = qnorm(1-alpha/2)

    B = za * sqrt(phat*(1-phat)/n)

    ll = phat - B - 1/ (2*n)
    ul = phat + B + 1/ (2*n)
    c(ll,ul)
  }
  if (method =="Clopper-Pearson Exact" ){
    ans = exactci(r, n, conflev)
  } else if (method =="Wilson Score") {
    ans = wilsonci(r, n, conflev)
  } else if (method =="Wilson Score with Continuity") {
    ans = score_contci(r, n, conflev)
  } else if (method == "Asymptotic Normal"){
    ans = simpleci(r, n, conflev)
  } else if (method == "Asymptotic Normal with Continuity"){
    ans = simple_contci(r, n, conflev)
  }
  return(ans)
}

