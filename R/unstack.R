#'
#' Unstack a Spectra2D Object into a n x F1*F2 matrix
#'
#' @export
#' @noRd
#'

.unstack <- function(spectra) {
  vectorizeByRow <- function(IN) { # Helper function from HandyStuff
    OUT <- rep(NA_real_, length(IN))
    nc <- ncol(IN)
    nr <- nrow(IN)
    a <- seq(1, length(IN), nc)
    b <- a + nc - 1
    for (n in 1:length(a)) {
      OUT[a[n]:b[n]] <- IN[n, ]
    }
    OUT
  }

  # Unstack the data into a new matrix

  no.samples <- length(spectra$names)
  no.F1 <- length(spectra$F1)
  no.F2 <- length(spectra$F2)
  no.pts <- no.F2 * no.F1
  M <- matrix(NA_real_, nrow = no.samples, ncol = no.pts)

  for (i in 1:no.samples) M[i, ] <- vectorizeByRow(spectra$data[[i]])

  M
}
