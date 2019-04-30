#' Remove Rows and Columns That are All Zeros in a Spectra2D Object
#'
#' @export
#' @noRd
#' 
.trimZeros.Spectra2D <- function(spectra){
		
  args2 <- as.list(match.call())[-1]
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  Z <- .findZeros(spectra)
  if (all(lengths(Z)) == 0L) {
  	message("No zeros to trim")
  	return(spectra)
  }
  
  # Now adjust the Spectra2D object
  spectra$F1 <- spectra$F1[-Z$rowZeros]
  spectra$F2 <- spectra$F2[-Z$colZeros]
  for (i in 1:length(spectra$names)) {
  	spectra$data[[i]] <- spectra$data[[i]][-Z$rowZeros, - Z$colZeros]
  }
  chkSpectra(spectra)
  spectra
}

