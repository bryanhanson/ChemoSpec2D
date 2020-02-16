#'
#'
#' Normalize a Spectra2D Object
#'
#' This function carries out normalization of the spectra in a
#' \code{\link{Spectra2D}} object.  The current options are:
#' \itemize{
#'   \item \code{"zero2one"} normalizes each 2D spectrum to a [0 \ldots{} 1] scale.
#'   \item \code{"minusPlus"} normalizes each 2D spectrum to a [-1 \ldots{} 1] scale.
#'   \item \code{"TotInt"} normalizes each 2D spectrum so that the total area is one.
#' }
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}} to be normalized.
#'
#' @param method Character string giving the method for normalization.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @export
#' @seealso \code{\link{centscaleSpectra2D}} for another means of scaling.
#'
#' @examples
#'
#' data(MUD1)
#' MUD1n <- normSpectra2D(MUD1)
#' MUD1b <- removeFreq(MUD1, remF2 = 2.5 ~ 3.5)
#' MUD1bn <- normSpectra2D(MUD1b)
normSpectra2D <- function(spectra, method = "zero2one") {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  ok <- c("zero2one", "TotInt", "minusPlus")
  if (!method %in% ok) stop("Invalid method specified")
  ns <- length(spectra$names)

  # normalize each 2D spectrum to a [0 ... 1] range:

  if (method == "zero2one") {
    for (i in 1:ns) {
      rMin <- min(spectra$data[[i]], na.rm = TRUE)
      rMax <- max(spectra$data[[i]], na.rm = TRUE)
      spectra$data[[i]] <- .rescale(spectra$data[[i]], 0.0, 1.0, rMin, rMax)
    }
  }

  # normalize each 2D spectrum to a [-1 ... 1] range:
  if (method == "minusPlus") {
    for (i in 1:ns) {
      rMin <- min(spectra$data[[i]], na.rm = TRUE)
      rMax <- max(spectra$data[[i]], na.rm = TRUE)
      spectra$data[[i]] <- .rescale(spectra$data[[i]], -1.0, 1.0, rMin, rMax)
    }
  }

  if (method == "TotInt") {
    for (i in 1:ns) {
      spectra$data[[i]] <- spectra$data[[i]] / sum(spectra$data[[i]], na.rm = TRUE)
    }
  }

  chkSpectra(spectra)
  return(spectra)
}
