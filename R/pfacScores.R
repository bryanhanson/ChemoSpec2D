#'
#' Plot Scores from a PARAFAC Analysis of a Spectra2D Object
#' 
#' Plots a 2D spectrum stored in a \code{\link{Spectra2D}} object. UPDATE
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param pfac An An object of class \code{parafac}.
#'
#' @param which An integer specifying the scores to plot.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#'
#' @keywords hplot
#'
#' @seealso Please see \code{\link{pfacSpectra2D}} for examples.
#' 
#' @export
#'
#' @importFrom graphics plot
#'
pfacScores <- function(spectra, pfac, which = c(1, 2), ...) {
	
	if (length(which) != 2L) stop("which must give two scores to plot")
	chkSpectra2D(spectra)
	
	plot(pfac$C[,which[1]], pfac$C[,which[2]], col = spectra$colors, pch = 20, ...)
}