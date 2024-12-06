% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{The Bootstrap Interval Function}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, rnd = 2, ...)
}
\arguments{
\item{iter}{number of samples for resampling}

\item{x}{sample to resample from}

\item{fun}{function to apply on resamples}

\item{alpha}{probability outside the intended bootstrap interval}

\item{cx}{character expansion number}

\item{rnd}{number of decimal places to round to in the graph}

\item{...}{other graphical arguments for the histogram}
}
\value{
a named list with the interval, x, fun, the vector of results from fun on x, and the point estimate
}
\description{
The Bootstrap Interval Function
}
\examples{
myboot2(iter = 10000, x = rbeta(20, 3, 4), fun = "var", alpha = 0.2, cx = 1.5, rnd = 3)
}
