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
	
	if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
	if (class(pfac) != "parafac") stop("pfac argument was not a parafac object")
	if (length(which) != 2L) stop("Please supply two scores to plot (argument 'which')")
	chkSpectra2D(spectra)

	# See stackoverflow.com/a/46289614/633251 for the concepts re: argument handling
	
	# Use a sensible xlab and ylab if none provided
	args <- as.list(match.call()[-1])
	if (!("xlab" %in% names(args))) {
		xlab <- paste("Component", which[1], sep = " ")
		args <- c(args, list(xlab = xlab))
		}
	if (!("ylab" %in% names(args))) {
		ylab <- paste("Component", which[2], sep = " ")
		args <- c(args, list(ylab = ylab))
		}

	# Update & clean the argument list
	
	args <- c(args, list(x = pfac$C[,which[1]], y = pfac$C[,which[2]], col = spectra$colors, pch = 20))
	args["spectra"] <- NULL
	args["pfac"] <- NULL
	if ("which" %in% names(args)) args["which"] <- NULL

	# Now create the plot
	
	do.call(plot, args)
}