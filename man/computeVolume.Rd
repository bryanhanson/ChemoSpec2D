% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeVolume.R
\name{computeVolume}
\alias{computeVolume}
\title{Compute the Volume of a Specified Shift Range in a 2D Spectrum}
\usage{
computeVolume(spectra, F2range = NULL, F1range = NULL)
}
\arguments{
\item{spectra}{An object of S3 class \code{\link{Spectra2D}}.}

\item{F2range}{A formula giving a frequency range. May include
"low" or "high" representing the extremes of the spectra.  Values below or above the range of
F2 are tolerated without notice and are handled as \code{min} or \code{max}, respectively.}

\item{F1range}{As for \code{F2range}, but for the F1 dimension.}
}
\value{
A numeric vector of volumes, one for each spectrum.
}
\description{
This function takes a range of frequencies for each dimension, and calculates
the volume of the enclosed region.
}
\examples{
data(MUD1)
tst <- computeVolume(MUD1, F2range = 3 ~ 4, F1range = 55 ~ 70)
}
\author{
Bryan A. Hanson, DePauw University.
}
\keyword{utilities}
