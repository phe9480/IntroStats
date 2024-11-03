#' Confidence Interval (Two-Sided) of mean difference with known or unknown variance
#'
#' This function calculates the CI of mean difference with known or unknown variance assuming normal distribution.
#'
#' @param x1bar Sample 1 mean
#' @param n1 Sample 1 size
#' @param x1 Sample 1. Default NULL.
#' @param sd1 Known standard deviation for population 1.
#' @param s1 sample 1 standard deviation.
#' @param x2bar Sample 2 mean
#' @param n2 Sample 2 size
#' @param x2 Sample 2. Default NULL.
#' @param sd2 Known standard deviation for population 2.
#' @param s2 sample 2 standard deviation.
#' @param conflev Two-sided confidence level
#' @param known.variance TRUE or FALSE. Default FALSE, i.e. t distribution.
#' @param equal.variance TRUE or FALSE. Default TRUE, i.e. pooled t
#' @param uneqvar.method "Welch" or "Cochran". This option is applicable only
#' in the scenario of unequal and unknown variances. Default "Welch".
#'
#' @examples
#' #Example (1). The sample x is provided.
#' x1 = rnorm(100, mean=10, sd=2)
#' x2 = rnorm(100, mean=12, sd=2.2)
#'
#' ci.mu.diff(x1=x1, x2=x2, conflev=0.95, known.variance=FALSE, equal.variance=TRUE)
#' ci.mu.diff(x1=x1, x2=x2, conflev=0.95, known.variance=FALSE, equal.variance=FALSE)
#'
#' ci.mu.diff(x1=x1, x2=x2, sd1=2, sd2=2, conflev=0.95, known.variance=TRUE, equal.variance=TRUE)
#' ci.mu.diff(x1=x1, x2=x2, sd1=2, sd2=2, conflev=0.95, known.variance=TRUE, equal.variance=FALSE)
#'
#' #Example (2). Sample mean and n are provided.
#'
#' ci.mu.diff(x1bar=16, n1=20, s1 = 5, x2bar=12, n2=20, s2 = 4.5, conflev=0.95,
#' known.variance=FALSE, equal.variance=TRUE)
#'
#' ci.mu.diff(x1bar=16, n1=20, s1 = 5, x2bar=12, n2=20, s2 = 4.5, conflev=0.95,
#' known.variance=FALSE, equal.variance=FALSE)
#'
#' ci.mu.diff(x1bar=16, n1=20, sd1 = 5, x2bar=12, n2=20, sd2 = 5, conflev=0.95,
#' known.variance=TRUE, equal.variance=TRUE)
#'
#' ci.mu.diff(x1bar=16, n1=20, sd1 = 5, x2bar=12, n2=20, sd2 = 4.5, conflev=0.95,
#' known.variance=TRUE, equal.variance=FALSE)
#'
#' ci.mu.diff(x1bar=16, n1=20, s1 = 5, x2bar=12, n2=20, s2 = 4.5, conflev=0.95,
#' known.variance=FALSE, equal.variance=FALSE, uneqvar.method="Cochran")
#'
#' ci.mu.diff(x1bar=16, n1=20, s1 = 5, x2bar=12, n2=20, s2 = 4.5, conflev=0.95,
#' known.variance=FALSE, equal.variance=FALSE, uneqvar.method="Welch")
#'
#' @export
#'
ci.mu.diff <- function(x1=NULL, x1bar, n1, s1, sd1=NULL,
                       x2=NULL, x2bar, n2, s2, sd2=NULL,
                       known.variance=FALSE, equal.variance=TRUE, conflev=0.95,
                       uneqvar.method = "Welch"){

  alpha <- 1-conflev
  if(!is.null(x1)){x1bar = mean(x1); s1=sd(x1); n1=length(x1)}
  if(!is.null(x2)){x2bar = mean(x2); s2=sd(x2); n2=length(x2)}

  o <- list()
  if (equal.variance & known.variance){
    if (sd1 != sd2 || is.null(sd1)){stop("Error: sd1 and sd2 must be equal when equal.variance is TRUE and known variance.")}
    se <- sqrt(sd1^2/n1+sd2^2/n2)
    q <- qnorm(1-alpha/2)
  } else if (equal.variance & !known.variance){
    sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2)
    sp <- sqrt(sp2)
    se <- sp*sqrt(1/n1+1/n2)
    q <- qt(1-alpha/2, df=n1+n2-2)
  } else if (!equal.variance & known.variance) {
    if (is.null(sd1) || is.null(sd2)){stop("Error: sd1 and sd2 must be provided. ")}
    se <- sqrt(sd1^2/n1+sd2^2/n2)
    q <- qnorm(1-alpha/2)
  } else if (!equal.variance & !known.variance){
    a1 = s1^2/n1; a2 = s2^2/n2

    if (uneqvar.method == "Welch"){
      A = (a1 + a2)^2
      B = a1^2/(n1-1) + a2^2/(n2-1)
      Delta = A/B
      q <- qt(1-alpha/2, df=Delta)
      o$df <- Delta
    }
    if (uneqvar.method == "Cochran"){
      t1 <- qt(1-alpha/2, df=n1-1)
      t2 <- qt(1-alpha/2, df=n2-1)
      q <- (a1*t1 + a2*t2) / (a1+a2)
    }
    se <- sqrt(a1+a2)
    o$q = q
    o$uneqvar.method = uneqvar.method
  }

  diff <- x1bar - x2bar
  L <- diff - q*se
  U <- diff + q*se
  o$CI = c(L, U); o$mean.diff = diff; o$se.mean.diff=se
  o$known.variance = known.variance
  o$equal.variance = equal.variance
  if (!equal.variance && !known.variance){o$uneqvar.method=uneqvar.method}
  if (equal.variance && !known.variance){o$sp=sp}

  return(o)
}
