#'
#' Inspect Levels for Contour Plots of Spectra2D Objects
#' 
#' Given a \code{Spectra2D} object, this function will assist in selecting levels
#' for preparing contour and image type plots.  The entire range of the data is
#' used in the histogram so you can choose levels that are suitable for any
#' particular spectrum.  Any of the arguments to \code{\link{calcLvls}} can be used
#' to capture the computed levels, or you can choose by inspection.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param loadings Logical.  If a loadings entry is present, draw the histogram for it.
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
inspectLvls <- function(spectra, loadings = FALSE, ...) {

  .chkArgs(mode = 21L)
  
  if (!loadings) lvls <- calcLvls(unlist(spectra$data), showHist = TRUE, ...)
  
  if (loadings) {
  	load <- "loadings" %in% spectra$names
  	if (!load) stop("This Spectra2D object did not contain loadings.\n\t
  	  You need to run miaLoadings or pfacLoadings first.")
  	if (load) lvls <- calcLvls(
  	  spectra$data[[which(spectra$names == "loadings")]], showHist = TRUE, ...)
   }
   
  invisible(lvls)
  
} # end of inspectLvls
