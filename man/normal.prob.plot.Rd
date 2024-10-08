% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal.prob.plot.R
\name{normal.prob.plot}
\alias{normal.prob.plot}
\title{Plot Shade Under a Density Curve}
\usage{
normal.prob.plot(y, main = "Normal Probability Plot", ...)
}
\arguments{
\item{y}{sample data of a continuous variable}

\item{...}{additional graphical parameters pass to plot}
}
\value{
A dataframe with normal scores (expected) and original sorted sample
}
\description{
This function plots the F distribution density to illustrate the features.
}
\examples{
y <- rnorm(100, mean=10, sd=3)
normal.prob.plot(y, col="turquoise")

}
