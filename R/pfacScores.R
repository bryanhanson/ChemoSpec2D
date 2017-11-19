#'
#' Plot Scores from a PARAFAC Analysis of a Spectra2D Object
#' 
#' Plots scores from the PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' The scores are the values returned in matrix \code{C} in the \code{parafac}
#' object.  This matrix has dimensions no. samples x no. of requested components.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param pfac An object of class \code{parafac}.
#'
#' @param which An integer specifying two scores to plot.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
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
	
	if (length(which) != 2L) stop("Please supply two scores to plot (argument 'which')")
	chkSpectra2D(spectra)
	
	plot(pfac$C[,which[1]], pfac$C[,which[2]], col = spectra$colors, pch = 20, ...)
}