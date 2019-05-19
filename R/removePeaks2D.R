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
#' in the input data matrices.  See \code{\link{removeFreq}} for a way to shrink
#' the data set without introducing \code{NA}s.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} from which to
#' remove selected peaks.
#'
#' @param remF2 A formula giving the range of frequencies to be set to \code{NA}.  May include
#' "low" or "high" representing the extremes of the spectra.    Values outside the range of
#' F2 are tolerated without notice and are handled \code{min} or \code{max}.See the examples.
#'
#' @param remF1 As for \code{remF2}.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @seealso \code{\link[ChemoSpecUtils]{removeFreq}}.
#'
#' @export
#'
#' @examples
#' # Note we will set contours a bit low to better
#' # show what is going on.
#'
#' data(MUD1)
#' mylvls <- seq(-0.3, 0.3, 0.1)
#' mylvls[4] <- 0.05
#'
#' plotSpectra2D(MUD1, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7: Complete Data Set")
#'
#' MUD1a <- removePeaks2D(MUD1, remF2 = 2.5 ~ 4)
#' sumSpectra(MUD1a)
#' plotSpectra2D(MUD1a, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7\nRemoved Peaks: F2 2.5 ~ 4")
#'
#' MUD1b <- removePeaks2D(MUD1, remF2 = low ~ 2)
#' sumSpectra(MUD1b)
#' plotSpectra2D(MUD1b, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7\nRemoved Peaks: F2 low ~ 2")
#'
#' MUD1c <- removePeaks2D(MUD1, remF1 = high ~ 23)
#' sumSpectra(MUD1c)
#' plotSpectra2D(MUD1c, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7\nRemoved Peaks: F1 high ~ 23")
#'
#' MUD1d <- removePeaks2D(MUD1, remF2 = 2.5 ~ 4, remF1 = 45 ~ 55)
#' sumSpectra(MUD1d)
#' plotSpectra2D(MUD1d, which = 7, lvls = mylvls,
#'   main = "MUD1 Sample 7\nRemoved Peaks: F2 2.5 ~ 4 & F1 45 ~ 55")
#'
removePeaks2D <- function(spectra, remF2 = NULL, remF1 = NULL) {

	.chkArgs(mode = 21L)
	chkSpectra(spectra)
	if (is.null(remF2) & is.null(remF1)) stop("Nothing to remove")
		
	# Set peaks to NA as requested
	
	if (!is.null(remF2)) { # F2 dimension: sorted F2 runs low...high
		if (!inherits(remF2, "formula")) stop("remF2 must be a formula")
		limits <- .getLimits(spectra, "F2", remF2)
		toss <- ((spectra$F2 >= limits[1]) & (spectra$F2 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][,rev(toss)] <- NA	# rev needed since 0 in lr corner
	}

	if (!is.null(remF1)) { # F1 dimension: sorted F1  runs low...high
		if (!inherits(remF1, "formula")) stop("remF1 must be a formula")
		limits <- .getLimits(spectra, "F1", remF1)
		toss <- ((spectra$F1 >= limits[1]) & (spectra$F1 <= limits[2]))
		for (i in 1:length(spectra$data)) spectra$data[[i]][toss, ] <- NA	
	}

	chkSpectra(spectra)		
	return(spectra)
	}

