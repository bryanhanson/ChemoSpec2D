#'
#' Convert a \code{Spectra2D} Object to a \code{Spectra} Object by Linearizing
#' 
#' A utility function which takes a \code{Spectra2D} object and linearizes it.
#' Each 2D spectrum corresponding to a sample is linearized by taking each F1 slice
#' and assembling them one after the other into a vector.  The
#' frequency axis is replaced with an index (though it is stored as a real vector
#' to satisfy the definition of a \code{\link{ChemoSpec}Spectra} object).
#' The frequency vector will be equal to the length of the F2 dimension times
#' the length of the F1 dimension.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#' 
#' @return A object of S3 class \code{\link{Spectra}}.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities
#' 
#' @export
#'
#' @importFrom ChemoSpec chkSpectra
#'
toChemoSpec <- function(spectra) {
	
	chkSpectra2D(spectra)
	
	# Helper function from HandyStuff

	vectorizeByRow <- function(IN) {
		OUT <- rep(NA_real_, length(IN))
		nc <- ncol(IN)
		nr <- nrow(IN)
		a <- seq(1, length(IN), nc)
		b <- a + nc - 1
		for (n in 1:length(a)) {
			OUT[a[n]:b[n]] <- IN[n,]
			}
		OUT
		}

	no.samples <-length(spectra$names)
	no.F1 <- length(spectra$F1)
	no.F2 <- length(spectra$F2)
	no.pts <- no.F2 * no.F1
	
	SO <- list()
	SO$freq <- as.numeric(1:no.pts)
	SO$data <- matrix(NA_real_, nrow = no.samples, ncol = no.pts)
	SO$names <- spectra$names
	SO$groups <- spectra$groups
	SO$colors <- spectra$colors
	SO$sym <- rep(20, no.samples)
	SO$alt.sym <- rep("a", no.samples)
	SO$unit <- c("index", spectra$unit[3])
	SO$desc <- paste(spectra$desc, "LINEARIZED", sep = " ")
	class(SO) <- "Spectra"
	
	# Fill the data matrix
	
	for (i in 1:no.samples) SO$data[i,] <- vectorizeByRow(spectra$data[[i]])

	chkSpectra(SO)
	
	return(SO)
	}

