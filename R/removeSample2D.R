
#'
#' @describeIn removeGroup2D Remove samples from a \code{Spectra2D} object
#'
#' @export removeSample2D
#'
removeSample2D <- function(spectra, rem.sam) {
	
	if (missing(spectra)) stop("No spectral data provided")
	if (missing(rem.sam)) stop("Nothing to remove")
	chkSpectra2D(spectra)
	
	# remove the requested samples by name or number
	# BE CAREFUL: greping can catch more than you think!
	
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
	
	sn <- names(spectra)
	tn <- c("F2", "F1", "data", "names", "groups", "unit", "desc")
	extra <- setdiff(sn, tn)
	if (length(extra) > 0) {
		msg <- paste("Additional data was found:", extra, "and not modified\n", sep = " ")
		message(msg)
		message("If these are per sample data, you may have to manually edit them")
		msg <- paste("The removal indices are:", rem.sam, sep = " ")
		message(msg)
		
		}

	if (length(spectra$names) == 0) warning("You have removed all your samples!")

	chkSpectra2D(spectra)
	spectra

	}

