#' z-score
#'
#' This function calculates the z score.
#'
#' @param x A numeric array that consists of all possible observations in an entire finite population
#'
#' @examples
#' #Example.
#' z.score(c(1,2,2,3))
#'
#' z.score(c(1,2,2,3), mu=2.5, sigma=1)
#'
#' @export
#'
z.score <- function(x, mu=NULL, sigma=NULL) {
  if (is.null(mu)){mu = mean(x)}
  if (is.null(sigma)){sigma = pop.sd(x)}

  (x - mu) / sigma
}

