#' Remove Rows and Columns That are All Zeros in a Spectra2D Object
#'
#' @export
#' @noRd
#'  
.trimZeros <- function(spectra) {
	UseMethod(".trimZeros")
}

