#'
#' Inspect Levels for Contour Plots of Spectra2D Objects
#' 
#' Given a \code{Spectra2D} object, this function will assist in selecting levels
#' for preparing contour and image type plots.  The entire range of the data is
#' used in the histogram so you can choose levels that are suitable for any
#' particular spectrum.  However, loading matrices are excluded unless specified 
#' as the input. Any of the arguments to \code{\link{calcLvls}} can be used
#' to capture the computed levels, or you can choose by inspection.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param loading Integer.  If a loadings entry is present, draw the histogram for it.
#'        For example, if \code{loading = 1} \code{Loading_1} is used.
#'
#' @param \dots Arguments to be passed downstream.
#'
#' @return A numeric vector giving the levels (invisibly).
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @export
#'
#' @keywords utilities
#'
#' @seealso See \code{\link{pfacSpectra2D}} for further examples.
#'
#' @examples
#' 
#' data(MUD1)
#' inspectLvls(MUD1, ylim = c(0, 50), main = "All MUD1 Data, mode = even")
#' inspectLvls(MUD1, ylim = c(0, 50), mode = "NMR",  main = "All MUD1 Data, mode = NMR")
#' 
inspectLvls <- function(spectra, loading = NULL, ...) {

  .chkArgs(mode = 21L)
  
  if (is.null(loading)) {
  	pat <- "Loading_"
  	toss <- grep(pat, spectra$names)
  	if (length(toss) > 0L) spectra <- removeSample(spectra, toss)
  	lvls <- calcLvls(unlist(spectra$data), showHist = TRUE, ...)
  }
  
  if (!is.null(loading)) {
  	pat <- paste("Loading", loading, sep = "_")
  	Ldone <- grep(pat, spectra$names)
  	if (length(Ldone) == 0) stop("This Spectra2D object did not contain loadings.\n\t
  	  You need to run miaLoadings or pfacLoadings first.")
  	lvls <- calcLvls(spectra$data[[Ldone]], showHist = TRUE, ...)
   }
   
  invisible(lvls)
  
} # end of inspectLvls
