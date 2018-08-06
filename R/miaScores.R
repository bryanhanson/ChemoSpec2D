#'
#' Plot Scores from a Multivariate Image Analysis (Tucker1) of a Spectra2D Object
#'
#' This function plots the scores from the multivariate image analysis
#' of a \code{\link{Spectra2D}} object.  A multivariate image analysis is the
#' same operation as a Tucker1 analysis.
#' You need to run \code{\link{miaSpectra2D}} before using this function.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param mia A list as returned by \code{\link[ThreeWay]{pcasup1}}..
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
#' @seealso Please see \code{\link{miaSpectra2D}} for more information and examples.
#' 
#' @export
#'
#' @importFrom graphics plot
#'
miaScores <- function(spectra, mia, which = c(1, 2), ...) {
	
	if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
	chkSpectra2D(spectra)
	if (length(which) != 2L) stop("Please supply two scores to plot (argument 'which')")

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
	
	args <- c(args, list(x = mia$C[,which[1]], y = mia$C[,which[2]], col = spectra$colors, pch = 20))
	args["spectra"] <- NULL
	args["mia"] <- NULL
	if ("which" %in% names(args)) args["which"] <- NULL

	# Now create the plot
	
	do.call(plot, args)
}