#' Draw Multimodal Distribution
#'
#' This function draws a multimodal distribution for illustration of statistical distributions.
#'
#' @param peaks.loc Peak location
#' @param width Spread of each peak
#' @param n Sample size
#' @param ... Optional graphical parameters
#'
#' @examples
#' #Example (1). Unimodal
#' draw.multi.modes(peaks.loc = c(5),  width=c(3),
#' xlim=c(-5, 15), ylim=c(0, 0.42), main="Peak: 5, width: 3")
#'
#' #Example (2). bimodal
#' draw.multi.modes(peaks.loc = c(5, 15),  width=c(1,3),
#' xlim=c(-5, 25), ylim=c(0, 0.25),
#' main="Peak: 5 15, width: 1 3")
#'
#' #Example (3). 3-modal distribution
#' draw.multi.modes(peaks.loc = c(5, 15, 25),  width=c(1,2,3),
#' xlim=c(-5, 35), ylim=c(0, 0.15),
#' main="Peak: 5 15 25, width: 1 2 3")
#'
#' @export
#'
draw.multi.modes = function(peaks.loc = c(3, 7, 12),  width = c(1,1,1),
                            n = c(1000,1000,1000), ...){
  n.modes = length(unique(peaks.loc), ...)

  x = NULL
  for (i in 1:n.modes){
    x = c(x, rnorm(n[i], mean=peaks.loc[i], sd=width[i]))
  }

  hist(x, col="turquoise", prob=TRUE, breaks = 20*n.modes, ...)
  lines(density(x), col = "blue", lwd = 2)
}

