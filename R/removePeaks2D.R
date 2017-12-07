#'
#'
#' Remove Peaks in a Spectra2D Object
#' 
#' This function sets peaks at specified frequencies in a \code{\link{Spectra2D}}
#' object to \code{NA}. This effectively removes these peaks from calculations of
#' contours which can speed things up and clarifies the visual presentation of data.
#' This function is useful for removing regions with large
#' interfering peaks (e.g. the water peak in 1H NMR), or regions that are primarily
#' noise.  This function leaves the frequency axes intact.  Note that the
#' \code{\link[multiway]{parafac}} function does not allow \code{NA}
#' in the input data matrices.  See \code{\link{removeFreq2D}} for a way to reduce
#' the data set without introducing \code{NA}s.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} from which to
#' remove selected peaks.
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
#' @seealso \code{\link{removeFreq2D}}.
#'
#' @export
#'
#' @examples
#'
#' data(MUD1)
#' plotSpectra2D(MUD1, lvls = c(0.25, 0.5, 0.75),
#'   main = "MUD1 Spectrum 1: Complete Data Set")
#'
#' # Note for this low resolution data set, some formulas like
#' # remF2 = 3 ~ 4 result in no change due to rounding.
#'
#' MUD1a <- removePeaks2D(MUD1, remF2 = 2 ~ 4)
#' sumSpectra2D(MUD1a)
#' plotSpectra2D(MUD1a, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Peaks: F2 2 ~ 4")
#'
#' MUD1b <- removePeaks2D(MUD1, remF2 = low ~ 5)
#' sumSpectra2D(MUD1b)
#' plotSpectra2D(MUD1b, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Peaks: F2 low ~ 5")
#'
#' MUD1c <- removePeaks2D(MUD1, remF1 = high ~ 12)
#' sumSpectra2D(MUD1c)
#' plotSpectra2D(MUD1c, , lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Peaks: F1 high ~ 12")
#'
#' MUD1d <- removePeaks2D(MUD1, remF2 = 2 ~ 4, remF1 = 4 ~ 6)
#' sumSpectra2D(MUD1d)
#' plotSpectra2D(MUD1d, lvls = c(0.25, 0.5, 0.75),
#'   main = "Removed Peaks: F2 2 ~ 4 & F1 4 ~ 6")
#'
removePeaks2D <- function(spectra, remF2 = NULL, remF1 = NULL) {

	if (missing(spectra)) stop("No spectral data provided")
	if (is.null(remF2) & is.null(remF1)) stop("Nothing to remove")
	chkSpectra2D(spectra)
		
	# Set peaks to NA as requested
	
	if (!is.null(remF2)) { # F2 dimension: sorted F2 runs low...high
		if (!is.formula(remF2)) stop("remF2 must be a formula")
		limits <- .getLimits(spectra, "F2", remF2)
		toss <- ((spectra$F2 >= limits[1]) & (spectra$F2 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][,rev(toss)] <- NA	# rev needed since 0 in lr corner
	}

	if (!is.null(remF1)) { # F1 dimension: sorted F1  runs low...high
		if (!is.formula(remF1)) stop("remF1 must be a formula")
		limits <- .getLimits(spectra, "F1", remF1)
		toss <- ((spectra$F1 >= limits[1]) & (spectra$F1 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][rev(toss), ] <- NA	
	}

	chkSpectra2D(spectra)		
	return(spectra)
	}

