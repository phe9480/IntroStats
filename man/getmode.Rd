% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getmode.R
\name{getmode}
\alias{getmode}
\title{Calculate the sample mode}
\usage{
getmode(x)
}
\arguments{
\item{x}{A numeric array}
}
\description{
This function calculates the mode of a data set. First, find the frequency of
each value in the data set. If no value occurs more than once, the data set
has no mode. Otherwise, mode is defined as the value that occurs with the
greatest frequency. If more than 1 value has the greatest frequency,
the function returns all of them.
}
\examples{
#Example.
getmode(c(1,2,2,3))

getmode(c(1,2,3))

getmode(c(1,2,2,3,3))

}
