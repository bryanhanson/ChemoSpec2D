#'
#' Shift the Spectra in a Spectra2D Object
#' 
#' Shift the spectra in a \code{\link{Spectra2D}} object manually.  During shifting, some rows
#' or columns are thrown away and new rows or columns are introduced.  These new entries may
#' be filled with zeros, or random noise.
#' \itemize{
#'   \item (+) \code{shiftF2} - shift right: trim right, fill left
#'   \item (-) \code{shiftF2} - shift left: trim left, fill right
#'   \item (+) \code{shiftF1} - shift up: trim top, fill bottom
#'   \item (-) \code{shiftF1} - shift down: trim bottom, fill top
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
#' @param fill Aligning spectra requires that at least some spectra be shifted left/right
#'        and up/down.  When a spectrum is shifted, spaces are opened that must be filled with something:
#'  \itemize{
#'    \item If \code{fill = "zeros"} the spaces are filled with zeros.
#'    \item If \code{fill = "rnorm"} the spaces are filled with random noise.
#'    \item If \code{fill = "noise"} the spaces are filled with an estimate of the noise from the
#'      original spectrum.
#'  }
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#' @examples
#' data(MUD2)
#' # Show the first two spectra, overlaid
#' 
#' mylvls <- seq(5, 35, 5)
#' plotSpectra2D(MUD2, which = 1:2, lvls = list(mylvls, mylvls),
#'   cols = list(rep("black", 7), rep("red", 7)),
#'   main = "MUD2 Sample 1 (black) & Sample 2 (red)")
#' 
# Now shift Sample 2
#' MUD2s <- shiftSpectra2D(MUD2, which = 2, shiftF1 = -2)
#' plotSpectra2D(MUD2s, which = 1:2, lvls = list(mylvls, mylvls),
#'   cols = list(rep("black", 7), rep("red", 7)),
#'   main = "MUD2 Sample 1 (black) & Sample 2 (red)\n(samples aligned/overlap)")
#'
#' 
shiftSpectra2D <- function(spectra, which = NULL, shiftF2 = 0L, shiftF1 = 0L, fill = "noise") {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  
  if (is.null(which)) stop("You must supply argument which")
  shiftF2 <- as.integer(shiftF2)
  shiftF1 <- as.integer(shiftF1)
  if ((shiftF2 == 0L) & (shiftF1 == 0L)) stop("Both shiftF2 and shiftF1 are zero")

  if (fill == "noise") NS <- .noiseSurface(spectra)
  if (fill != "noise") NS <- NULL

  A <- .makeArray2(spectra, which)
  dimnames(A) <- NULL
  A <- .shiftArray(A, xshift = shiftF2, yshift = shiftF1, fill = "zero", NS = NS)
  for (i in 1:length(which)) {
  	spectra$data[[ which[i] ]] <- A[i,,]
  }
  
  chkSpectra(spectra)
  spectra
}

