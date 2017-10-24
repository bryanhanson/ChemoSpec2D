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
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @seealso Please see \code{\link{ChemoSpec2D-package}} for examples.
#' 
#' @export plotSpectra2D
#'
#' @importFrom graphics abline contour rect
#'
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Only a single spectrum can be plotted")
  chkSpectra2D(spectra)

  M <- spectra$data[[which]]
  
  if (is.null(lvls)) {
  	lvls <- chooseLvls(M, n = 10, mode = "poslog", lambda = 0.2)
  	lvls <- lvls[-1]
  }
  
  contour(x = spectra$F2, y = spectra$F1, z = M,
    levels = lvls, drawlabels = FALSE,...)
}
