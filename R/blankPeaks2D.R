#'
#'
#' Blank Peaks in a Spectra2D Object
#' 
#' This function sets peaks at specified frequencies in a \code{\link{Spectra2D}}
#' object to \code{NA}. This effectively removes these peaks from calculations of
#' contours which can speed things up and clarifies the visual presentation of data.
#' This function is useful for removing regions with large
#' interfering peaks (e.g. the water peak in 1H NMR), or regions that are primarily
#' noise.
#' 
#' Note that the \code{\link[multiway]{parafac}} function does not allow \code{NA}
#' in the input data matrices.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} from which to
#' remove selected frequencies.
#'
#' @param remF2 A formula giving the range of frequencies to be set to \code{NA}.  May include
#' "low" or "high" representing the extremes of the spectra.  See the examples.
#'
#' @param remF1 As for \code{remF2}.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
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
#' # Note for this low resolution data set, setting
#' # remF2 = 3 ~ 4 results in no change due to rounding.
#'
#' MUD1a <- blankPeaks2D(MUD1, remF2 = 2 ~ 4)
#' sumSpectra2D(MUD1a)
#' plotSpectra2D(MUD1a, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Blank F2 2 ~ 4")
#'
#' MUD1b <- blankPeaks2D(MUD1, remF2 = low ~ 5)
#' sumSpectra2D(MUD1b)
#' plotSpectra2D(MUD1b, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Blank F2 low ~ 5")
#'
#' MUD1c <- blankPeaks2D(MUD1, remF2 = 6 ~ high, remF1 = 4 ~ 13)
#' sumSpectra2D(MUD1c)
#' plotSpectra2D(MUD1c, lvls = c(0.25, 0.5, 0.75),
#'   main = "Blank F2 6 ~ high & F1 4 ~ 13")
#'
blankPeaks2D <- function(spectra, remF2 = NULL, remF1 = NULL) {

	if (missing(spectra)) stop("No spectral data provided")
	if (is.null(remF2) & is.null(remF1)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	# The user may not know or think about whether F2 or F1 is ascending or descending
	# so we will try to get it right no matter how the user gives
	# the formula; e.g. 6 ~ 3 ought to be handled as 3 ~ 6.
	
	# Helper Function
	
	getLimits <- function(spectra, dim, form) {
		lhs <- form[[2]]
		rhs <- form[[3]]
		if (as.character(lhs) == "low") lhs <- min(spectra[[dim]])
		if (as.character(lhs) == "high") lhs <- max(spectra[[dim]]) 
		if (as.character(rhs) == "low") rhs <- min(spectra[[dim]])
		if (as.character(rhs) == "high") rhs <- max(spectra[[dim]])
		ans <- c(lhs, rhs)
		if (is.unsorted(ans)) ans <- rev(ans)
		return(ans) # should always give numeric values in order
	}
	
	# Now set peaks to NA as requested
	
	if (!is.null(remF2)) { # F2 dimension: sorted F2 runs e.g. 0...10
		if (!is.formula(remF2)) stop("remF2 must be a formula")
		limits <- getLimits(spectra, "F2", remF2)
		toss <- ((spectra$F2 > limits[1]) & (spectra$F2 < limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][,rev(toss)] <- NA	# rev needed since 0 in lr corner
	}

	if (!is.null(remF1)) { # F1 dimension: sorted F1 runs e.g. 0...10 unsorted F1 runs e.g. 10...0
		if (!is.formula(remF1)) stop("remF1 must be a formula")
		limits <- getLimits(spectra, "F1", remF1)
		toss <- ((spectra$F1 > limits[1]) & (spectra$F1 < limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][toss, ] <- NA	
	}

	chkSpectra2D(spectra)		
	return(spectra)
	}

