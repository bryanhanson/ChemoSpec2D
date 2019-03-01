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
#' @param which An integer specifying which spectrum to plot.  May be a vector.
#'
#' @param lvls A numeric vector specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{\link{calcLvls}}.  If
#'        argument \code{which} gives more than one spectrum to plot, then \code{lvls}
#'        must be a list of levels of \code{length(which)}.
#'
#' @param cols A vector of valid color designations.  If provided, must be of the
#'        the same length as \code{lvls} (i.e. each contour is a particular color).
#'        If \code{NULL}, defaults to using a scheme of up to nine values
#'        running from blue (low) to red (high), centered on green (zero).  If
#'        argument \code{which} gives more than one spectrum to plot, then \code{cols}
#'        must be a list of colors of \code{length(which)}.
#'
#' @param showNA Logical. Should the locations of peaks removed by \code{\link{removePeaks2D}}
#'        be shown?  If present, these are shown by a gray line at each frequency.
#'
#' @param showGrid Logical. If TRUE, show a dotted gray line at each tick mark.
#'
#' @param \ldots Additional parameters to be passed to the plotting routines.
#'
#' @section Warning:
#' One cannot remove frequencies from the interior of a 2D NMR data set and expect to get a meaningful
#' contour plot, because doing so puts unrelated peaks adjacent in the data set.
#' This would lead to contours being drawn that don't exist in the original data set.
#' This function will check for missing frequencies and stop if any are found.
#'
#' @section Scale:
#' You can view the color scale for the plot via \code{\link{showScale}}.
#'
#' @section Levels & Colors:
#' The number of levels and colors must match, and they are used 1 for 1.  If you
#' provide \code{n} colors, and no levels, the automatic calculation of levels may return
#' a number of levels other than \code{n}, in which case the function will override your colors and
#' assign new colors for the number of levels it computed (with a message).  To get
#' exactly what you want, specify both levels and colors in equal numbers.  Function
#' \code{\link{inspectLvls}} can help you choose appropriate levels.
#'
#' @section Overlaying Spectra:
#' If you specify more than one spectrum to plot, e.g. \code{which = c(1,2)}, then
#' arguments \code{lvls} and \code{cols} must be lists of levels and colors, one list
#' element for each spectrum to be plotted (if specified at all).  See the examples.
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
#' mylvls <- seq(-0.3, 0.3, 0.1)[-4]
#' plotSpectra2D(MUD1, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7")
#' plotSpectra2D(MUD1, which = c(1, 6), lvls = list(mylvls, mylvls),
#'   cols = list(rep("black", 6), rep("red", 6)),
#'   main = "MUD1 Sample 1 (black) & Sample 6 (red)")
#'
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, cols = NULL,
  showNA = TRUE, showGrid = FALSE, ...) {
	
  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  
  # Stop if there are frequencies missing from the interior, this is misleading
  dF1 <- spectra$F1[2] - spectra$F1[1]
  diffF1 <- diff(spectra$F1)
  for (i in 1:length(diffF1)) {
  	if (!isTRUE(all.equal(diffF1[i], dF1, scale = 1.0))) { # detects discontinuity
  		stop("Cannot plot: missing frequencies along F1")
  	}
  }

  dF2 <- spectra$F2[2] - spectra$F2[1]
  diffF2 <- diff(spectra$F2)
  for (i in 1:length(diffF2)) {
  	if (!isTRUE(all.equal(diffF2[i], dF2, scale = 1.0))) { # detects discontinuity
  		stop("Cannot plot: missing frequencies along F2")
  	}
  }

  # Figure out how many spectra will be plotted and fix up levels and colors
  # accordingly. Process provided lvls and cols, including default values of NULL.
  # .plotEngine expects a list for each
  # .plotEngine will compute defaults if NULL is passed

  
  if (length(which) == 1L) {
    lvls <- list(lvls) # list(NULL) works here
    cols <- list(cols)  	
  }

  if (length(which) > 1L) {
  	if (is.null(lvls)) lvls <- vector("list", length(which))
  	if (is.null(cols)) cols <- vector("list", length(which))  	
  }
  
  # Go plot
  op <- par(no.readonly = TRUE) # save to restore later
  par(mai = c(0.75, 0.5, 1.0, 0.75))
  .plotEngine(spectra = spectra, which = which,
    lvls = lvls, cols = cols, showGrid = showGrid, ...)
  
  # Show NAs if requested
  if (showNA) {
  	 	
  	NAs <- .findNA(spectra)
  	rNA <- NAs[[1]] # row NAs will be drawn as a horizontal line
  	cNA <- NAs[[2]] # col NAs will be drawn as a vertical line
  	
  	if (length(cNA) != 0) {
  		V <- cNA/length(spectra$F2)
  		abline(v = V, col = "gray98", lwd = 2)
  	}
  	  	
  	if (length(rNA) != 0) {
  		H <- 1 - rNA/length(spectra$F1)
  		abline(h = H, col = "gray98", lwd = 2)
  	}

  } # end of showNA
  
  # Add showScale = TRUE code?
    
  on.exit(par(op)) # restore original values
}
