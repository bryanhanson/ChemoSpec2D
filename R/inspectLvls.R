#'
#' Inspect Levels for Contour Plots of Spectra2D Objects
#' 
#' Given a \code{Spectra2D} object, this function will assist in selecting levels
#' for preparing contour and image type plots.
#' Any of the arguments to \code{\link{calcLvls}} can be used
#' to compute the levels, or you can choose your own by inspection.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which Integer.  The spectrum/spectra to be analyzed.  If a vector,
#'        the intensities are combined.
#'
#' @param \dots Arguments to be passed downstream to \code{\link{calcLvls}} and/or
#'        the plot function (e.g. \code{ylim}).
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
#' inspectLvls(MUD1, ylim = c(0, 300), main = "MUD1 Spectrum 1, mode = even")
#' inspectLvls(MUD1, ylim = c(0, 300), mode = "NMR",  main = "MUD1 Spectrum 1, mode = NMR")
#' 
inspectLvls <- function(spectra, which = 1, ...) {
  .chkArgs(mode = 21L)
  lvls <- calcLvls(unlist(spectra$data[[which]]), showHist = TRUE, ...)
  invisible(lvls)
}
