#'
#' Plot Spectra2D Object
#' 
#' Plots a 2D spectrum stored in a \code{\link{Spectra2D}} object.
#' This is primarily for inspection and for preparation of final plots.
#' If you need to do extensive exploration, you should probably go back
#' to the spectrometer.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer specifying which spectrum to plot.
#'
#' @param lvls An integer specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{chooseLvls}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return The chosen levels are returned invisibly, to aid in fine-tuning them.
#'         Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @seealso Please see \code{\link{ChemoSpec2D-package}} for examples.
#' 
#' @export
#'
#' @importFrom lattice contourplot lattice.getOption
#'
plotSpectra2D <- function(spectra, which = 1, lvls = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Only a single spectrum can be plotted")
  chkSpectra2D(spectra)

  M <- spectra$data[[which]]
  M <- t(M[nrow(M):1,]) # 90 cw prior to rotation in contourplot
  
  if (is.null(lvls)) lvls <- guessLvls(M)

# Helper function from https://stackoverflow.com/a/47210555/633251
# Puts F1 axis on right side by changing defaults
  myaxis <- function(side, labels = "default", ticks = "default", ...) {
    default <- lattice.getOption("default.args")$axis
    if (side %in% c("right","bottom")) {
      default(side = side, labels = "yes", ticks = "yes", ...)
    } else {
      default(side = side, labels = "no", ticks = "yes", ...)
    }
  }

  # Compute tick positions and limits
  # Little tweaks from concept testing
  F2st <- spectra$F2[1]
  F2end <- spectra$F2[length(spectra$F2)] + 1
  F2at <- seq(F2st, F2end, 1)
  F2lab <- as.character(seq(F2end, F2st, -1))
  F1st <- spectra$F1[1]
  F1end <- spectra$F1[length(spectra$F1)]
  F1at <- seq(F1st, F1end, 1)
  F1lab <- as.character(seq(F1end, F1st, -1))
  
  p <- contourplot(M, asp = 1,
    xlab = spectra$unit[1], ylab = "", ylab.right = spectra$unit[2],
    axis = myaxis,
    scales = list(
      x = list(at = F2at, labels = F2lab),
      y = list(at = F1at + 1, labels = F1lab),
      ...)
    )
  print(p)
  
  invisible(lvls)
}
