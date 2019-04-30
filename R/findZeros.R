#'
#' Find All Zero Columns and Rows in a Spectra2D Object
#' 
#' This function identifies any column or row that is composed of all zeros
#' (this arises due to shifting during alignment).
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @return A list with two elements giving the indices of zeros for rows and columns.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @importFrom stats na.omit
#' @noRd
#' @export
#'

.findZeros <- function(spectra) {

  .chkArgs(mode = 0L)

  nr <- nrow(spectra$data[[1]])
  nc <- ncol(spectra$data[[1]])
  ns <- length(spectra$names)
  rZ <- rep(NA_integer_, nr)
  cZ <- rep(NA_integer_, nc)

  # We will check all spectra separately and keep the longest set of zeros.
  # There is some redundancy here but there's probably no easy way to avoid it.
  
  for (i in 1:ns) {
	for (j in 1:nc) { # check along F2 for columns that are all zero
		if (all(spectra$data[[i]][,j] == 0.0)) cZ[j] <- j
	}
	for (k in 1:nr) { # check along F1 for rows that are all zero
		if (all(spectra$data[[i]][k, ] == 0.0)) rZ[k] <- k
	}
  }	

  rZ <- na.omit(rZ)
  attributes(rZ) <- NULL
  cZ <- na.omit(cZ)
  attributes(cZ) <- NULL
  
  return(list(rowZeros = rZ, colZeros = cZ))
}

