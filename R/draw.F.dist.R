#' Plot F distribution density to illustrate the features of F distribution
#'
#' This function plots the F distribution density to illustrate the features.
#' 
#' @param df1 Numerator degrees of freedom in F distribution
#' @param df2 Denominator degrees of freedom in F distribution
#' 
#' @return graph
#' 
#' @examples
#' #F(3, 100)
#' draw.F.dist(3, 100)
#' 
#' @export 
#'
draw.F.dist <- function(df1, df2){
  x <- seq(0, 20, by=0.01)
  plot(x, df(x, df1=df1, df2=df2), type="l", lwd=3, col="turquoise",
       main=paste("F(", df1, ", ", df2, ") Density Function", sep = ""),
       xlab="", ylab="Density")
}




