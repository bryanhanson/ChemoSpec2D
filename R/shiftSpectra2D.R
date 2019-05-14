#'
#' Shift the Spectra in a Spectra2D Object
#' 
#' Shift the spectra in a \code{\link{Spectra2D}} object manually.  During shifting, some rows
#' or columns are thrown away and new rows or columns are introduced.  These new entries may
#' be filled with zeros, or random noise.
#' \itemize{
#'   \item (+) x-shift - shift right: trim right, fill left
#'   \item (-) x-shift - shift left: trim left, fill right
#'   \item (+) y-shift - shift up: trim top, fill bottom
#'   \item (-) y-shift - shift down: trim bottom, fill top
#' }
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer specifying which spectra to shift.  May be a vector.
#'
#' @param shiftF2 Integer.  The number of data points to shift along the F2 dimension.  See Details.
#'
#' @param shiftF1 As per \code{shiftF2}, but for the F1 dimension.
#'
#' @param fill Character.  One of \code{"zero"} or \code{"rnorm"} describing how to fill
#'        the empty rows or columns that are created during shifting.  If \code{rnorm}
#'        you may wish to set the seed.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @export
#'
#'

shiftSpectra2D <- function(spectra, which = NULL, shiftF2 = 0L, shiftF1 = 0L, fill = "zero") {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  
  if (is.null(which)) stop("You must supply argument which")
  shiftF2 <- as.integer(shiftF2)
  shiftF1 <- as.integer(shiftF1)
  if ((shiftF2 == 0L) & (shiftF1 == 0L)) stop("Both shiftF2 and shiftF1 are zero")

  A <- .makeArray2(spectra, which)
  dimnames(A) <- NULL
  A <- .shiftArray(A, xshift = shiftF2, yshift = shiftF1, fill = "zero")
  for (i in 1:length(which)) {
  	spectra$data[[ which[i] ]] <- A[i,,]
  }
  spectra <- .trimZeros(spectra)
  
  chkSpectra(spectra)
  spectra
}

