% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fugu.R
\name{compose}
\alias{compose}
\title{Function Composition}
\usage{
compose(...)
}
\arguments{
\item{...}{a list of functions - all but the first must be single argument}
}
\value{
a function g st. g(a,b,c) == f1(f2(f3(a,b,c))) etc
}
\description{
Returns a new function F which threads its values through all
passed in functions
}
\keyword{functional}

