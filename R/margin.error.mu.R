#' Margin of Error for Population Mean
#'
#' This function calculates the margin of error for population mean
#' 
#' @param x sample
#' @param conflev Confidence level
#' @param s sample standard deviation
#' @param sigma population standard deviation 
#' @param n sample size 
#' @param method "Known Variance" or "Unknown Variance"
#' 
#' 
#' @return E
#' 
#' @examples
#' #Example
#' 
#' x = rnorm(30)
#' margin.error.mu(x=x, sigma=1, conflev=0.95, method="Known Variance")
#' margin.error.mu(n=length(x), sigma=1, conflev=0.95, method="Known Variance")
#' margin.error.mu(x=x, conflev=0.95, method="Unknown Variance")
#' margin.error.mu(s=sd(x), n=length(x), conflev=0.95, method="Unknown Variance")
#' 
#' 
#' @export 
#'
margin.error.mu <- function(x=NULL, conflev=0.95, s, sigma=NULL, n, method="Unknown Variance"){
  alpha = (1-conflev)/2
  if(!is.null(x)){s = sd(x); n=length(x)}
  if (method == "Known Variance"){
    if (is.null(sigma)){stop("sigma is required for method of known variance.")}
    q = qnorm(1-alpha/2)
    E = q*sigma/sqrt(n)
  } else {
    if (is.null(s)){stop("s is required for method of unknown variance.")}
    
    q = qt(1-alpha/2, df=n-1)
    E = q*s/sqrt(n)
  }
  return(E)
}
