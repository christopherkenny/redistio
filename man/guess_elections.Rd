% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guess.R
\name{guess_elections}
\alias{guess_elections}
\title{Guess which columns contain election data}
\usage{
guess_elections(shp)
}
\arguments{
\item{shp}{an \code{sf} tibble that you want to draw with}
}
\value{
a named \code{list} of columns
}
\description{
Guess which columns contain election data
}
\examples{
guess_elections(dc)
}
