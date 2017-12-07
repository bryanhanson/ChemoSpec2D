#'
#' Summarize a Spectra2D Object
#' 
#' Provides a summary of a \code{\link{Spectra2D}} object, essentially a more
#' spectroscopist-friendly version of \code{str()}.
#' 
#' Prior to summarizing, \code{\link{chkSpectra2D}} is run with confirm = FALSE.
#' If there are problems, warnings are issued to the console and the summary is
#' not done.  The
#' \code{\link{Spectra2D}} object is also checked to see if it contains data elements
#' beyond what is required.  If so, these extra elements are reported to the
#' console.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param ...  Arguments to be passed downstream. If \code{sumSpectra2D} thinks there is a
#' gap between every data point, add the argument \code{tolF1 = xx} and/or 
#' \code{tolF2 = xx} which will pass through to
#' the underlying functions and override the internal guessing at appropriate values.
#'
#' @return None.  Results printed at console.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#'
#' @importFrom stats median na.omit
#'
#  NOTE: na.omit used in a non-exported function; here for convenience
#'
#' @examples
#'
#' data(MUD1)
#' sumSpectra2D(MUD1)
#'
#' MUD1a <- removeFreq2D(MUD1, remF2 = 2 ~ 4, remF1 = 10 ~ 15)
#' sumSpectra2D(MUD1a)
#'
#' MUD1b <- removePeaks2D(MUD1, remF2 = 2 ~ 4, remF1 = 10 ~ 15)
#' sumSpectra2D(MUD1b)
#' plotSpectra2D(MUD1b)
#'
sumSpectra2D <- function(spectra, ...){
	
	# Lots of code ideas derived from ChemoSpec::sumSpectra
	
	chkSpectra2D(spectra)

	# Try to determine a sensible value for tol if none provided via the ...
	# Then analyze for gaps
	
	args <- names(as.list(match.call()[-1]))

	if (!("tolF1" %in% args)) {
		diffF1 <- diff(spectra$F1) 
		tolF1 <- abs(median(diffF1)) * 1.2 # ensures value is a bit larger than nominal resolution
		gF1 <- .check4Gaps(spectra$F1, tol = tolF1)	
		}
	
	if (!("tolF2" %in% args)) {
		diffF2 <- diff(spectra$F2) 
		tolF2 <- abs(median(diffF2)) * 1.2 # ensures value is a bit larger than nominal resolution
		gF2 <- .check4Gaps(spectra$F2, tol = tolF2)	
		}

	if ("tolF1" %in% args) gF1 <- .check4Gaps(spectra$F1, tol = tolF1)	
	if ("tolF2" %in% args) gF2 <- .check4Gaps(spectra$F2, tol = tolF2)	

	# Check for NAs in the matrices
	
	NAindx <- .findNA(spectra, retFreq = FALSE)
	foundNA<- FALSE
	if ((length(NAindx[[1]]) > 0) | (length(NAindx[[2]]) > 0)) foundNA <- TRUE
	
	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n\n", sep = "")
	
	cat("\tThe F2 dimension runs from ", spectra$F2[1], " to ", 
		spectra$F2[length(spectra$F2)], " ", spectra$unit[1], "\n", sep = "")
		
	if (nrow(gF2) > 1) {
		cat("\tThe F2 dimension has gaps. Here are the data chunks:\n\n")
		print(gF2)
		}
	cat("\n")
	
	cat("\tThe F1 dimension runs from ", spectra$F1[1], " to ", 
		spectra$F1[length(spectra$F1)], " ", spectra$unit[2], "\n", sep = "")
		
	if (nrow(gF1) > 1) {
		cat("\tThe F1 dimension has gaps. Here are the data chunks:\n\n")
		print(gF1)
		}
	cat("\n")
	
	if (foundNA) cat("\tNAs were found in the data matrices.  To see where, plotSpectra2D.\n\n")
	
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
	print(sumGroups2D(spectra))
	
	# Check for extra data and report if found
	cat("\n")
	if (length(spectra) > 8) .extraData(spectra)
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra2D'\n")
	}

