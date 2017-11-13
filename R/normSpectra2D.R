#'
#'
#' Normalize a Spectra2D Object
#' 
#' This function carries out normalization of the spectra in a
#' \code{\link{Spectra2D}} object.  There are currently two options:
#' \itemize{
#'   \item \code{"zero2one"} scales each 2D spectrum separately to a [0 \ldots{} 1] scale.
#'   \item \code{"TotInt"} scales each 2D spectrum separately so that the total area is one.
#' }
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}} to be normalized.
#'
#' @param method One of \code{"TotInt"} or \code{"zero2one"} giving
#' the method for normalization.  Other methods may be added in the future.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
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
#' nMUD <- normSpectra2D(MUD1)
#'
normSpectra2D <- function(spectra, method = "TotInt") {
	
	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra2D(spectra)

# normalize each 2D spectrum to a [0...1] range:

	if (method == "zero2one") {
		for (i in 1:length(spectra$names)) {
			rMin <- min(spectra$data[[i]])
			spectra$data[[i]] <- spectra$data[[i]] - rMin
			rMax <- max(spectra$data[[i]])
			spectra$data[[i]] <- spectra$data[[i]]/rMax
			}
		}

	if (method == "TotInt") {
		for (i in 1:length(spectra$names)) {
			spectra$data[[i]] <- spectra$data[[i]]/sum(spectra$data[[i]])
			}
		}

	chkSpectra2D(spectra)
	return(spectra)
	}
