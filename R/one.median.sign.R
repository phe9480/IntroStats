#' Sign Test of One Population Median
#'
#' This function performs the sign test of one population median.
#' 
#' @param x sample data
#' @param m0 median under H0
#' @param tail type of test: "two", "left", "right"
#' 
#' @return p value
#' 
#' @examples
#' x <- rnorm(10, mean=10, sd=3)
#' one.median.sign(x=x, m0=10, tail="two")
#' #compared to one sample t-test
#' one.mean.t(x=x, mu0=10, tail="two")
#' 
#' @export 
#'
one.median.sign <- function(x, m0, tail="two") {
    n_pos <- sum (x > m0)
    n_neg <- sum (x < m0)
    n <- n_pos + n_neg
    
    if (tail == "left") {
      p <- pbinom(n_pos, size = n, prob=0.5)
    } else if (tail == "right"){
      p <- pbinom(n_neg, size=n, prob=0.5)
    } else if (tail == "two"){
      p <- 2*min(pbinom(n_neg, size=n, prob=0.5), 
                 pbinom(n_pos, size=n, prob=0.5))
    }
    p <- min(p, 1)
    return(p)
}
  




