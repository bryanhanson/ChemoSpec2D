#'
#'
#' Center and Scale a Spectra2D Object Along the Samples Dimension
#'
#' This function will optionally center, and optionally scale, a \code{Spectra2D} object along the
#' samples dimension (i.e. this is pixel-wise scaling in the language of multivariate
#' image analysis). Several scaling options are available.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param scale A character string indicating the type of scaling to apply.  One of
#' \code{c("autoscale", "Pareto", "log", "log10")}. For the log functions, centering is not carried
#' out since logarithm is not defined for negative values.
#'
#' @param center Logical.  Should the spectra be centered before possibly scaling?  Will give an error
#' if \code{center = TRUE} and a log function is requested for scaling.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references
#' R. Bro and A. K. Smilde "Centering and Scaling in Component Analysis" J. Chemometrics
#' vol. 17 pgs 16-33 (2003).
#'
#' @keywords utilities
#'
#' @export
#'
#' @seealso \code{\link{normSpectra2D}} for another means of scaling.
#'
#' @examples
#'
#' data(MUD1)
#' tst <- centscaleSpectra2D(MUD1)
centscaleSpectra2D <- function(spectra, center = FALSE, scale = "noscale") {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  nS <- length(spectra$names)
  nF1 <- length(spectra$F1)

  if (center) {
    if (!requireNamespace("matrixStats", quietly = TRUE)) {
      stop("You must install package matrixStats to use this function")
    }

    A1 <- .makeArray(spectra) # frontal slabs contain spectra$data entries
    A2 <- aperm(A1, perm = c(3, 2, 1))

    # center the data
    if (center) for (i in 1:nF1) A2[, , i] <- A2[, , i] - matrixStats::colMeans2(A2[, , i], na.rm = TRUE)

    if (scale == "autoscale") {
      for (i in 1:nF1) A2[, , i] <- A2[, , i] / matrixStats::colSds(A2[, , i], na.rm = TRUE)
    }

    if (scale == "Pareto") {
      for (i in 1:nF1) A2[, , i] <- A2[, , i] / sqrt(matrixStats::colSds(A2[, , i], na.rm = TRUE))
    }

    if ((scale == "log") | (scale == "log10")) stop("Cannot take log of centered data")

    A3 <- aperm(A2, c(3, 2, 1)) # restore original orientation

    for (i in 1:nS) {
      spectra$data[[i]] <- A3[, , i]
      dimnames(spectra$data[[i]]) <- NULL # strip dimensions from array/aperm
    }
  } # end of center = TRUE

  if (!center) {
    # log scaling is done on an entire spectrum/matrix
    if (scale == "log") {
      for (i in 1:nS) {
        if (any(spectra$data[[i]] < 0.0)) stop("Negative values present in spectrum. Cannot take log.")
        spectra$data[[i]] <- log(spectra$data[[i]])
      }
    }

    if (scale == "log10") {
      for (i in 1:nS) {
        if (any(spectra$data[[i]] < 0.0)) stop("Negative values present in spectrum. Cannot take log.")
        spectra$data[[i]] <- log10(spectra$data[[i]])
      }
    }
  } # end of center = FALSE

  chkSpectra(spectra)
  return(spectra)
}
