#'
#'
#' Verify the Integrity of a Spectra2D Object
#' 
#' Utility function to verify that the structure of a \code{\link{Spectra2D}}
#' object (an S3 object) is internally consistent. This function can be used
#' after manual editing of a \code{\link{Spectra2D}} object.  However,
#' in most cases rather than
#' directly manipulating a \code{\link{Spectra2D}} object, one should manipulate
#' it via \code{\link{removeGroup2D}}, \code{\link{blankPeaks2D}},
#' or \code{\link{removeSample2D}}.
#' 
#' This function is similar in spirit to \code{\link{validObject}} in the S4
#' world.  When used at the console, and the object is OK, no message is
#' written unless \code{confirm = TRUE}.  At the console, if there is a
#' problem, messages are issued regardless of the value of \code{confirm}.
#' When used in a function, this function is silent (assuming \code{confirm =
#' FALSE}) unless there is a problem.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}} to be checked.
#' 
#' @param confirm Logical indicating whether or not to write the results to the
#' console, as would be desirable for interactive use.
#' 
#' @return None; messages will be printed at the console if there is a problem.
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords classes utilities
#' 
#' @export chkSpectra2D
#'
#' @importFrom utils str
#'
#' @examples
#'
#' data(MUD1)
#'
#' chkSpectra2D(MUD1, confirm = TRUE)
#'
chkSpectra2D <- function(spectra, confirm = FALSE) {
	
	# Check classes of each element
	
	if (missing(spectra)) stop("No object of supposed class Spectra2D provided")
	trouble <- FALSE
	if (!class(spectra) == "Spectra2D") { warning("The object provided was not of class Spectra2D"); trouble <- TRUE }
	if (!class(spectra$F2) == "numeric") { warning("The F2 frequency data are not numeric"); trouble <- TRUE }
	if (!class(spectra$F1) == "numeric") { warning("The F1 frequency data are not numeric"); trouble <- TRUE }
	if (!class(spectra$data) == "list") { warning("Data is not a list"); trouble <- TRUE }
	if (!class(spectra$names) == "character") { warning("The sample names are not character type"); trouble <- TRUE }
	if (!class(spectra$units) == "character") { warning("The units are not character type"); trouble <- TRUE }
	if (!class(spectra$desc) == "character") { warning("The description is not character type"); trouble <- TRUE }
	if (!class(spectra$groups) == "factor") { warning("The assigned groups are not factor type"); trouble <- TRUE }
	if (!class(spectra$colors) == "character") { warning("The assigned colors are not character type"); trouble <- TRUE }
	
	# Check that F2 and F1 are sorted ascending

	if (is.unsorted(spectra$F2)) {warning("F2 frequency data are not sorted ascending"); trouble <- TRUE }
	if (is.unsorted(spectra$F1)) {warning("F1 frequency data are not sorted ascending"); trouble <- TRUE }

	# Check to make sure that data matrices have the same dim
	
	ns <- length(spectra$names)
	dims <- matrix(NA_integer_, ncol = 2, nrow = ns)
	rownames(dims) <- spectra$names
	colnames(dims) <- c("F2", "F1")
	for (i in 1:ns) {
		if (!is.matrix(spectra$data[[i]])) stop("spectra$data entries should be matrices")
		if (!is.numeric(spectra$data[[i]])) stop("spectra$data entries should be numeric matrices")
		dims[i,] <- c(ncol(spectra$data[[i]]), nrow(spectra$data[[i]]))
	}
	
	Ucol1 <- length(unique(dims[,1])) == 1L # TRUE/FALSE
	Ucol2 <- length(unique(dims[,2])) == 1L
	
	if (!Ucol1 | !Ucol2) {
		message("Data matrices do not have the same dimensions.")
		print(dims)
	}
	
	# Check that the relationships between each element are correct
		
	F2 <- length(spectra$F2)
	F1 <- length(spectra$F1)
	dd <- dim(spectra$data[[1]])
	g <- length(spectra$groups)
	nc <- length(spectra$colors)
	# note: ns was defined earlier as length(spectra$names)
	
	if (!identical(F1, dd[1])) { warning("Length(F1) != nrow(data)"); trouble <- TRUE }
	if (!identical(F2, dd[2])) { warning("Length(F2) != ncol(data)"); trouble <- TRUE }
	if (!identical(ns, g)) { warning("The dimensions don't make sense (names, group)"); trouble <- TRUE }
	if (!identical(ns, nc)) { warning("The dimensions don't make sense (names, colors)"); trouble <- TRUE }
	
		
	# Check for extra list elements and report

	if ((length(spectra) > 8 ) && (confirm)) {
		reqd <- c("F2", "F1", "data", "names", "groups", "colors", "unit", "desc")
		spc <- spectra[!names(spectra) %in% reqd]
		message(">>> Extra data was found in the spectra object:")
		str(spc)
		}
	
	# Wrap up
	
	if ((!trouble) && (confirm)) message(">>> You must be awesome: These spectra look great!")
	if (trouble) {
		message("*** There seem to be one or more problems with these spectra!")
		stop("Sorry, we can't continue this way: It's not me, it's you!")
		}
	
	}

