#'
#' Compute the Volume of a Specified Shift Range in a 2D Spectrum
#'
#' This function takes a range of frequencies for each dimension, and calculates
#' the volume of the enclosed region.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param F2range A formula giving a frequency range. May include
#' "low" or "high" representing the extremes of the spectra.  Values below or above the range of
#' F2 are tolerated without notice and are handled as \code{min} or \code{max}, respectively.
#'
#' @param F1range As for \code{F2range}, but for the F1 dimension.
#'
#' @return A numeric vector of volumes, one for each spectrum.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#' @export
#' @examples
#' data(MUD1)
#' tst <- computeVolume(MUD1, F2range = 3 ~ 4, F1range = 55 ~ 70)
computeVolume <- function(spectra, F2range = NULL, F1range = NULL) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  if (is.null(F1range)) stop("You must supply argument F1range")
  if (is.null(F2range)) stop("You must supply argument F2range")

  if (!inherits(F1range, "formula")) stop("F1range must be a formula")
  if (!inherits(F2range, "formula")) stop("F2range must be a formula")

  F1limits <- .getLimits(spectra, "F1", F1range)
  F2limits <- .getLimits(spectra, "F2", F2range)

  F1keep <- ((spectra$F1 >= F1limits[1]) & (spectra$F1 <= F1limits[2]))
  F2keep <- ((spectra$F2 >= F2limits[1]) & (spectra$F2 <= F2limits[2]))

  # subset to desired region, then sum
  ns <- length(spectra$names)
  for (i in 1:ns) spectra$data[[i]] <- spectra$data[[i]][F1keep, F2keep, drop = FALSE]
  vols <- rep(NA_integer_, ns)
  for (i in 1:ns) vols[i] <- sum(spectra$data[[i]])
  vols
}
