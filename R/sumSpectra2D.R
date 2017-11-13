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
#' @param ...  Arguments to be passed downstream.
#'
#' @return None.  Results printed at console.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#'
#' data(MUD1)
#'
#' sumSpectra2D(MUD1)
#'
sumSpectra2D <- function(spectra, ...){
	
	chkSpectra2D(spectra)

	# Now print main summary to console
	
	cat("\n", spectra$desc, "\n\n")
	cat("\tThere are ", length(spectra$names), " spectra in this set.\n", sep = "")
	cat("\tThe F2 dimension runs from ", spectra$F2[1], " to ", 
		spectra$F2[length(spectra$F2)], " ", spectra$unit[1], "\n", sep = "")
	cat("\tThe F1 dimension runs from ", spectra$F1[1], " to ", 
		spectra$F1[length(spectra$F1)], " ", spectra$unit[2], "\n", sep = "")
	cat("\tThe spectra are divided into", length(levels(spectra$groups)), "groups:", "\n\n")
	print(sumGroups2D(spectra))
	
	# Check for extra data and report if found
	
	sn <- names(spectra)
	tn <- c("F2", "F1", "data", "names", "groups", "unit", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("\n\tAdditional data was found:", extra, "\n", sep = " ")
		cat(msg)
		}
	
	cat("\n*** Note: this data is an S3 object of class 'Spectra2D'\n")
	}

