% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n.p.CI.lowerbound.R
\name{n.p.CI.lowerbound}
\alias{n.p.CI.lowerbound}
\title{Sample Size calculation for achieving lower bound of CI greater than a threshold}
\usage{
n.p.CI.lowerbound(
  n = 15:30,
  p0 = 0.2,
  p1 = 0.4,
  conflev = 0.95,
  method = "Clopper-Pearson Exact"
)
}
\arguments{
\item{n}{A range of sample size}

\item{p0}{A proportion under H0}

\item{p1}{Proportion under H1}

\item{conflev}{Confidence level, eg, 0.95}

\item{method}{Confidence interval method. Options include "Clopper-Pearson Exact",
"Wilson Score", "Wilson Score with Continuity", "Asymptotic Normal", and
"Asymptotic Normal with Continuity".}
}
\value{
An object with values including
\itemize{
\item  n.target: Target sample size achieving the lower bound >= p0
\item  table: A table of r, n, CI, phat
}
}
\description{
This function calculates the sample size required achieving lower bound of CI greater than a threshold
}
\examples{
Example
n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Clopper-Pearson Exact")

n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Wilson Score with Continuity")

n.p.CI.lowerbound(n=15:30, p0=0.2, p1=0.4, conflev=0.95, method="Asymptotic Normal")

}
