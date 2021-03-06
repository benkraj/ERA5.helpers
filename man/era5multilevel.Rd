% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/era5multilevel.R
\name{era5multilevel}
\alias{era5multilevel}
\title{Multi level ERA5 extraction.}
\usage{
era5multilevel(file, variable, lat, long)
}
\arguments{
\item{file}{Filename}

\item{variable}{ERA5 variable name, in my case RH, wind direction, and wind speed}

\item{lat}{Latitude of point (it will find closest ERA5 grid point)}

\item{long}{Longitude of point}
}
\description{
This is a (mostly helper) function for extracting out multi level (height) era5 data
}
\examples{
era5multilevel("rh_201803.nc", "rh", 12.5, 11.5)
}
\keyword{ERA5}
