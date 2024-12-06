% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myncurve.R
\name{myncurve}
\alias{myncurve}
\title{The Normal CDF Plot Function}
\usage{
myncurve(mu, sigma, a)
}
\arguments{
\item{mu}{mean}

\item{sigma}{standard deviation}

\item{a}{probability upper limit}
}
\value{
list containing mu, sigma, and P(Y <= a)
}
\description{
The Normal CDF Plot Function
}
\examples{
myncurve(mu = 10, sigma = 5, a = 4)
}
