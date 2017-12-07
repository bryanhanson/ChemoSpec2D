#'
#' Plot a Spectra2D Object
#' 
#' Plots a 2D spectrum stored in a \code{\link{Spectra2D}} object.
#' This is primarily for inspection and for preparation of final plots.
#' If you need to do extensive exploration, you should probably go back
#' to the spectrometer.
#' 
#' @section Warning:
#' One cannot remove frequencies from the interior of a 2D NMR data set and expect to get a meaningful
#' contour plot, because doing so puts unrelated peaks adjacent in the data set.
#' This would lead to contours being drawn that don't exist in the original data set.
#' This function checks for missing frequencies.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer specifying which spectrum to plot.
#'
#' @param lvls A numeric vector specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{\link{guessLvls}}.
#'
#' @param showNA Logical. Should the locations of peaks removed by \code{\link{removePeaks2D}}
#'        be shown?  If present, these are show by a red line at each frequency.
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
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, showNA = TRUE, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Only a single spectrum can be plotted")
  
  # Stop if there are frequencies missing from the interior, this is misleading
  if (length(unique(diff(spectra$F1))) != 1) stop("There were missing frequencies along F1")
  if (length(unique(diff(spectra$F2))) != 1) stop("There were missing frequencies along F2")
  chkSpectra2D(spectra)

  lvls <- list(lvls)
  
  op <- par(no.readonly = TRUE) # save to restore later
  par(mai = c(1, 0.5, 1, 1))
  .plotEngine(spectra, which, lvls, ...)
  
  if (showNA) {
  	
  	NAs <- .findNA(spectra, retFreq = TRUE)
  	
  	# NOTE: examples probably work because the scales start at zero
  	
  	# Process rows
  	# rNA are indices from left-to-right, but axis labels right-to-left
  	# which affects where we draw the lines
  	rNA <- NAs[[1]]
  	
  	if (length(rNA) != 0) {
  		V <- rNA/diff(range(spectra$F2))
  		abline(v = V, col = "red", lwd = 2)
  	}
  	
  	# Process columns
  	# cNA are indices from top-to-bottom, which corresponds to labels
  	# which affects where we draw the lines
  	cNA <- NAs[[2]]
  	
  	if (length(cNA) != 0) {
  		H <- 1 - cNA/diff(range(spectra$F1))
  		abline(h = H, col = "red", lwd = 2)
  	}

  } # end of showNA
    
  on.exit(par(op)) # restore original values
}
