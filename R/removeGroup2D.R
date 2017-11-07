#'
#'
#' Remove Groups or Samples from a Spectra2D Object
#' 
#' Removes specified groups or samples from a \code{\link{Spectra2D}} object.
#' 
#' Both functions will report if extra data elements are found.  These will
#' probably need to be edited manually.  The indices reported to the console
#' can be helpful in this regard.
#'
#' If \code{rem.sam} is a character vector, the sample
#' names are grepped for the corresponding values.  \code{rem.group}
#' also uses grep.  Remember that the
#' grepping process is greedy, i.e. grepping for "XY" find not only "XY" but
#' also "XYZ".
#'
#' Unused levels in \code{$groups} are dropped.
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
#' @export removeGroup2D
#'
#' @describeIn removeGroup2D Remove groups from a \code{Spectra2D} object
#'
#' @seealso Please see \code{\link{ChemoSpec2D-package}} for examples.
#'
removeGroup2D <- function(spectra, rem.group) {
	
	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.group)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	# BE CAREFUL: greping can catch more than you think!
	
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

	sn <- names(spectra)
	tn <- c("F2", "F1", "data", "names", "groups", "unit", "colors", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("Additional data was found:", extra, "-- but not modified\n", sep = " ")
		message(msg)
		message("If these are per sample data, you may have to manually edit them")
		message("The removal indices are:")
		print(rem.group)
		}
	
	if (length(spectra$groups) == 0) warning("You have removed all your samples!")


	chkSpectra2D(spectra)
	spectra

	}

