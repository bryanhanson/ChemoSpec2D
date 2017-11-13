#'
#' Plot Spectra2D Object
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
#' @param lvls An integer specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{chooseLvls}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @export
#'
#' @importFrom graphics axis box mtext
#'
#' @examples
#'
#' data(MUD1)
#'
#' plotSpectra2D(MUD1)
#'
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Only a single spectrum can be plotted")
  chkSpectra2D(spectra)

  lvls <- list(lvls)
  
  plotEngine(spectra, which, lvls, ...)
    
}
