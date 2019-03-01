#'
#' Plot a Slice of a Spectra2D Object
#' 
#' Plots a slice of a 2D spectrum stored in a \code{\link{Spectra2D}} object.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which A single integer specifying which 2D spectrum from which to
#'   plot the slice.
#'
#' @param F2 A single frequency to plot.
#'
#' @param F1 A single frequency to plot. 
#'
#' @param showGrid Logical. If TRUE, show a dotted gray line at each x axis tick mark.
#'
#' @param \ldots Additional parameters to be passed to the plotting routines.
#'
#' @section Note:
#' Only one of \code{F2} or \code{F1} should be given.
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
#' plotSlice(MUD1, F1 = 25, main = "Slice @ F1 = 25 ppm")
#'
plotSlice <- function(spectra, which = 1, F2 = NULL, F1 = NULL, showGrid = TRUE, ...) {
	
  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  
  if ((!is.null(F2)) & (!is.null(F1))) stop("Only one of F2 or F1 may be given")
  
  if (!is.null(F2)) {
  	if (length(F2) != 1L) stop("F2 must be a single value")
  	msg <- "F2 is out of range"
  	if (F2 > max(spectra$F2)) stop(msg)
  	if (F2 < min(spectra$F2)) stop(msg)
  	F2i <- findInterval(F2, spectra$F2)
  	F2i <- length(spectra$F2) - F2i + 1
  	limy <- range(unlist(spectra$data))
  	plot(spectra$F1, spectra$data[[which]][,F2i], type = "l",
  	  ylim = limy, xaxt = "n",
  	  xlab = spectra$unit[2], ylab = "intensity (entire data set)", ...)
    
    # Custom ticks similar to .plotEngine
    thres <- 15.0
    F1ticks <- .computeTicks(spectra$F1)
    if (diff(range(spectra$F1)) > thres) F1lab <- formatC(F1ticks, digits = 0, format = "f")
    if (diff(range(spectra$F1)) <= thres) F1lab <- formatC(F1ticks, digits = 2, format = "f")
  	axis(side = 1, at = F1ticks, labels = F1lab, cex.axis = 0.75)
    if (showGrid) abline(v = F1ticks, col = "gray", lty = "dotted")
  	
  }

  if (!is.null(F1)) {
  	if (length(F1) != 1L) stop("F1 must be a single value")
  	msg <- "F1 is out of range"
  	if (F1 > max(spectra$F1)) stop(msg)
  	if (F1 < min(spectra$F1)) stop(msg)
  	F1i <- findInterval(F1, spectra$F1)
  	# F1i <- length(spectra$F1) - F1i + 1
  	limy <- range(unlist(spectra$data))
  	plot(spectra$F2, spectra$data[[which]][F1i,], type = "l",
  	  ylim = limy, xaxt = "n",
  	  xlab = spectra$unit[1], ylab = "intensity (entire data set)", ...)
    
    # Custom ticks similar to .plotEngine
    thres <- 15.0
    F2ticks <- .computeTicks(spectra$F2)
    if (diff(range(spectra$F2)) > thres) F2lab <- rev(formatC(F2ticks, digits = 0, format = "f"))
    if (diff(range(spectra$F2)) <= thres) F2lab <- rev(formatC(F2ticks, digits = 2, format = "f"))
  	axis(side = 1, at = F2ticks, labels = F2lab, cex.axis = 0.75)
    if (showGrid) abline(v = F2ticks, col = "gray", lty = "dotted")
  	
  }
  
}

