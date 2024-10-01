#' Calculate the sample mode
#'
#' This function calculates the mode of a data set. First, find the frequency of
#' each value in the data set. If no value occurs more than once, the data set
#' has no mode. Otherwise, mode is defined as the value that occurs with the
#' greatest frequency. If more than 1 value has the greatest frequency, 
#' the function returns all of them.
#'
#' @param x A numeric array
#'
#' @examples
#' #Example.
#' getmode(c(1,2,2,3))
#'
#' getmode(c(1,2,3))
#'
#' getmode(c(1,2,2,3,3))
#'
#' @export
#'
getmode <- function(x) {
  u <- unique(x)
  if (length(u) == length(x)){
    return(NULL)
  } else{
    m <- max(tabulate(match(x, u)))
    u[tabulate(match(x, u))==m]
  }
}

