#'
#'
#' Remove Frequencies from a Spectra2D Object
#' 
#' This function deletes specified frequencies from a \code{\link{Spectra2D}} object.
#' This function is useful for removing regions with large interfering peaks
#' (e.g. the water peak in 1H NMR), or regions that are primarily noise.
#' It can also be used to isolate particular regions for further study.
#'
#' @section Warning:
#' One cannot remove frequencies from the interior of a 2D NMR data set and expect
#' to get a meaningful contour plot, because doing so puts unrelated peaks adjacent
#' in the data set. This would lead to contours being drawn that don't exist in the
#' original data set. However, one can remove data from the interior and run a PARAFAC
#' analysis on the result, using the spectrum as an abstract object (that is, the
#' spectrum may not plottable, but the resulting scores are still meaningful).
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} from which to
#' extract peaks.
#'
#' @param remF2 A formula giving the range of frequencies to be extracted.  May include
#' "low" or "high" representing the extremes of the spectra.  See the examples.
#'
#' @param remF1 As for \code{remF2}.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{removePeaks2D}} for another way to remove data.
#'
#' @keywords utilities
#'
#' @importFrom plyr is.formula
#'
#' @export
#'
#' @examples
#'
#' data(MUD1)
#' plotSpectra2D(MUD1, lvls = c(0.25, 0.5, 0.75),
#'   main = "MUD1 Spectrum 1: Complete Data Set")
#'
#' MUD1a <- removeFreq2D(MUD1, remF2 = 2 ~ 4)
#' sumSpectra2D(MUD1a) # cannot plot, results would be misleading
#'
#' MUD1b <- removeFreq2D(MUD1, remF2 = low ~ 5)
#' sumSpectra2D(MUD1b)
#' plotSpectra2D(MUD1b, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Frequencies: F2 low ~ 5")
#'
#' MUD1c <- removeFreq2D(MUD1, remF1 = low ~ 5)
#' sumSpectra2D(MUD1c)
#' plotSpectra2D(MUD1c, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Frequencies: F1 low ~ 5")
#'
#' MUD1d <- removeFreq2D(MUD1, remF2 = 6 ~ high, remF1 = 4 ~ 13)
#' sumSpectra2D(MUD1d) # not plotted, results would be misleading
#'
removeFreq2D <- function(spectra, remF2 = NULL, remF1 = NULL) {

	if (missing(spectra)) stop("No spectral data provided")
	if (is.null(remF2) & is.null(remF1)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	# Subset data as requested
	
	if (!is.null(remF2)) { # F2 dimension: sorted F2 runs e.g. 0...10
		if (!is.formula(remF2)) stop("remF2 must be a formula")
		limits <- .getLimits(spectra, "F2", remF2)
		toss <- !((spectra$F2 >= limits[1]) & (spectra$F2 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]] <- spectra$data[[i]][,rev(toss), drop = FALSE]
		spectra$F2 <- spectra$F2[toss] # rev needed since 0 in lr corner
	}

	if (!is.null(remF1)) { # F1 dimension: sorted F1 runs e.g. 0...10 unsorted F1 runs e.g. 10...0
		if (!is.formula(remF1)) stop("remF1 must be a formula")
		limits <- .getLimits(spectra, "F1", remF1)
		toss <- !((spectra$F1 >= limits[1]) & (spectra$F1 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]] <- spectra$data[[i]][rev(toss),, drop = FALSE]
		spectra$F1 <- spectra$F1[toss]
	}

	chkSpectra2D(spectra)		
	return(spectra)
	}

