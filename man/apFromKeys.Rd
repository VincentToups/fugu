% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fugu.R
\name{apFromKeys}
\alias{apFromKeys}
\title{Apply from a list of keys}
\usage{
apFromKeys(f, keys, v)
}
\arguments{
\item{f}{a function}

\item{v}{a value}

\item{fs}{a list of functions to cleave over v}
}
\value{
q like f(fs1(v),fs2(v),...)
}
\description{
Apply f to the results of indexing v with fs
}
\keyword{functions}

