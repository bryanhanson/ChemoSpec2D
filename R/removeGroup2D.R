#'
#'
#' Remove Groups or Samples from a Spectra2D Object
#' 
#' Removes specified groups or samples from a \code{\link{Spectra2D}} object.
#' 
#' Both functions will report if extra data elements are found, which will
#' need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.sam} is a character vector, the sample
#' names are grepped for the corresponding values.  \code{rem.group}
#' also uses grep.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' Unused levels in \code{$groups} are dropped by \code{removeGroup2D}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param rem.group A character vector giving the groups to be removed.
#'
#' @param rem.sam Either an integer vector specifying the samples to be
#' removed, or a character vector giving the sample names to be removed.
#' 
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#'
#' @describeIn removeGroup2D Remove groups from a \code{Spectra2D} object
#'
#' @examples
#'
#' data(MUD1)
#' sumSpectra2D(MUD1)
#'
#' MUD1a <- removeGroup2D(MUD1, "GroupA")
#' sumGroups2D(MUD1a)
#'
#' MUD1b <- removeSample2D(MUD1, "Sample1")
#' sumSpectra2D(MUD1b)
#'
removeGroup2D <- function(spectra, rem.group) {
	
	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	k <- c()
	if (is.character(rem.group)) {
		for (n in 1:length(rem.group)) {
			more <- grep(rem.group[n], spectra$groups)
			k <- c(k, more)
			}
		rem.group <- k
		}

	if (max(rem.group) > length(spectra$groups)) stop("Groups to remove are out of range")
	if (length(rem.group) == 0L) stop("No matching groups found to remove")
	
	spectra$data <- spectra$data[-rem.group]
	spectra$names <- spectra$names[-rem.group]
	spectra$colors <- spectra$colors[-rem.group]
	spectra$groups <- spectra$groups[-rem.group, drop = TRUE]

	if (length(spectra) > 8) .extraData(spectra, rem.group)
	
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")

	chkSpectra2D(spectra)
	
	return(spectra)
	}

