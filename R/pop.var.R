#' Population variance for a finite population
#'
#' This function calculates the population variance for a finite population.
#'
#' @param x A numeric array that consists of all possible observations in an entire finite population
#'
#' @examples
#' #Example.
#' pop.var(c(1,2,2,3))
#'
#'
#' @export
#'
pop.var <- function(x) {
  mu = mean(x)
  sum((x-mu)^2)/length(x)
}

