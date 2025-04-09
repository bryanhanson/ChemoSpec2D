#'
#' Compute the Noise Surface of a Spectra2D Object
#'
#' Follows the method described in the reference, section 2.2 "Noise Level Calculation".
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @return An array (of matrices/2D spectra), essentially the data element of a
#'         \code{\link{Spectra2D}} object, but each entry is the corresponding noise surface.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @references R. Koradi, M. Billeter, M. Engeli, P. Guntert and K. Wuthrich. \emph{J. Mag. Res.}
#'             vol. 135 pgs. 288-297 (1998).
#'
#'
#' @noRd
#'

.noiseSurface <- function(spectra) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  if (!requireNamespace("RcppRoll", quietly = TRUE)) {
    stop("You must install package RcppRoll to use this function")
  }

  nF1 <- length(spectra$F1)
  nF2 <- length(spectra$F2)
  nS <- length(spectra$names)

  # Helper function to compute noise for a single 2D spectrum
  single2DSpecNoise <- function(M) {
    # compute per slice noise, along each dimension
    nF1 <- nrow(M)
    nF2 <- ncol(M)

    F1noise <- rep(NA_real_, nF1)
    nF1sample <- ceiling(0.05 * nF1)
    for (i in 1:nF1) F1noise[i] <- min(RcppRoll::roll_sdl(M[i, ], nF1sample), na.rm = TRUE)
    F1noise <- F1noise * 3

    F2noise <- rep(NA_real_, nF2)
    nF2sample <- ceiling(0.05 * nF2)
    for (i in 1:nF2) F2noise[i] <- min(RcppRoll::roll_sdl(M[, i], nF2sample), na.rm = TRUE)
    F2noise <- F2noise * 3

    # compute base noise and update slice noise
    baseNoise <- min(F1noise, F2noise) # eqn 1 in ref
    F1noise <- sqrt(F1noise^2 - baseNoise^2) # eqn 2 in ref
    F2noise <- sqrt(F2noise^2 - baseNoise^2)

    # compute noise across entire matrix
    Mnoise <- matrix(NA_real_, nF1, nF2)
    for (i in 1:nF1) { # eqn 3 in ref (first piece; don't understand 2nd expression)
      for (j in 1:nF2) {
        Mnoise[i, j] <- sqrt(F1noise[i] * F2noise[j] + baseNoise^2)
      }
    }
    Mnoise
  } # end of single2DSpecNoise

  Noise <- array(NA_real_, dim = c(nS, nF1, nF2))
  for (i in 1:nS) Noise[i, , ] <- single2DSpecNoise(spectra$data[[i]])
  Noise
}
