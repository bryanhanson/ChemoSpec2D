
#'
#' @describeIn removeGroup2D Remove samples from a \code{Spectra2D} object
#'
#' @export removeSample2D
#'
removeSample2D <- function(spectra, rem.sam) {
	
	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	# Remove the requested samples by name or number
	
	k <- c()
	if (is.character(rem.sam)) {
		for (n in 1:length(rem.sam)) {
			more <- grep(rem.sam[n], spectra$names)
			k <- c(k, more)
			}
		rem.sam <- k
		}

	if (max(rem.sam) > length(spectra$names)) stop("Samples to remove are out of range")

	spectra$data <- spectra$data[-rem.sam]
	spectra$names <- spectra$names[-rem.sam]
	spectra$colors <- spectra$colors[-rem.sam]
	spectra$groups <- spectra$groups[-rem.sam, drop = TRUE]
	
	if (length(spectra) > 8) .extraData(spectra, rem.sam) # report on any extra data in the Spectra2D object
	
	if (length(spectra$names) == 0) warning("You have removed all your samples!")

	chkSpectra2D(spectra)
	spectra

	}

