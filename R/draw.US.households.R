#' Draw Sample Distribution of U.S. Household Size
#'
#' This function draws the sample distribution of U.S. household size for illustration of statistical distributions.
#'
#' @param size Number of households to draw
#' @param p Population probabilities of household sizes 1, 2, ..., 7+.
#' @param ... Optional graphical parameters
#'
#' @examples
#' #Example.
#' #Population Distribution
#' p = c(0.27, 0.34, 0.15, 0.14, 0.06, 0.03, 0.01)
#' barplot(p, col="turquoise", space=0,
#' names.arg = c("1", "2", "3", "4", "5","6","7+"), ylim=c(0, 0.4),
#' main="Population Distribution (All US Households)", xlab = "Household Size")
#'
#' #Sample Distribution
#' draw.US.households(size=100, main="Sample 1 Distribution (100 US Households)")
#' draw.US.households(size=100, main="Sample 2 Distribution (100 US Households)")
#' draw.US.households(size=100, main="Sample 3 Distribution (100 US Households)")
#' draw.US.households(size=100, main="Sample 4 Distribution (100 US Households)")
#' draw.US.households(size=100, main="Sample 5 Distribution (100 US Households)")
#' draw.US.households(size=100, main="Sample 6 Distribution (100 US Households)")
#' draw.US.households(size=1000, main="Sample 7 Distribution (1000 US Households)")
#' draw.US.households(size=100000, main="Sample 8 Distribution (100,000 US Households)")
#'
#' @export
#'
draw.US.households = function(size=100,
                              p=c(0.27, 0.34, 0.15, 0.14, 0.06, 0.03, 0.01),
                              ...){
  n = rmultinom(n=1, size=size, prob=p)
  p.hat = n / size
  barplot(c(p.hat), col="turquoise", space=0,
          names.arg = c("1", "2", "3", "4", "5","6","7+"),
          ylim=c(0, 0.4), xlab="Household Size", ...)
}




