#'
#' Plot a Spectra2D Object
#' 
#' Plots a 2D spectrum stored in a \code{\link{Spectra2D}} object.
#' This is primarily for inspection and for preparation of final plots.
#' If you need to do extensive exploration, you should probably go back
#' to the spectrometer.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer specifying which spectrum to plot.
#'
#' @param lvls A numeric vector specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{chooseLvls}.
#'
#' @param \ldots Additional parameters to be passed to the plotting routines.
#'
#' @return Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @export
#'
#' @examples
#'
#' data(MUD1)
#'
#' plotSpectra2D(MUD1, main = "MUD1", lvls = c(0.25, 0.5, 0.75))
#'
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Only a single spectrum can be plotted")
  chkSpectra2D(spectra)

  lvls <- list(lvls)
  
  plotEngine(spectra, which, lvls, ...)

}
