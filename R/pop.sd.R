#' Population variance for a finite population
#'
#' This function calculates the population variance for a finite population.
#'
#' @param x A numeric array that consists of all possible observations in an entire finite population
#'
#' @examples
#' #Example.
#' pop.sd(c(1,2,2,3))
#'
#'
#' @export
#'
pop.sd <- function(x) {
  mu = mean(x)
  sqrt(sum((x-mu)^2)/length(x))
}

