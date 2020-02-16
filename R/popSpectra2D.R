#' Plain Old PCA (POP) of Spectra2D Objects
#'
#' This function unstacks a \code{Spectra2D} object and conducts IRLBA
#' PCA on it.
#' To unstack, each F1 slice (parallel to F2) is concatenated one after the other
#' so that each 2D spectrum becomes a 1D spectrum.  The length of this spectrum will be
#' equal to the length of the F2 dimension times the length of the F1 dimension.
#' PCA is performed on the collection of 1D spectra (one spectrum from each 2D spectrum).
#' The IRLBA algorithm is used because the resulting matrix (n samples  in rows x F1 * F2 columns)
#' can be very large, and other PCA algorithms can struggle.
#'
#' The scale choice \code{autoscale} scales the columns by their standard
#' deviation.  \code{Pareto} scales by the square root of the standard
#' deviation. \code{"autoscale"} is called "standard normal variate" or "correlation matrix PCA"
#' in some literature.  This action is performed on the unstacked matrix, as is centering.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param choice A character string indicating the choice of scaling.  One of
#' \code{c("noscale"}, \code{"autoscale"}, \code{"Pareto")}.
#'
#' @param n Integer. The number of components desired.
#'
#' @param ... Other parameters to be passed to \code{\link[irlba]{prcomp_irlba}}.
#'
#' @return An object of classes \code{prcomp}, \code{pop} and \code{computed_via_irlba}
#' modified to include a list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (used to annotate
#' plots).
#'
#' @references J. Baglama and L. Reichel, "Augmented Implicitly Restarted Lanczos
#' Bidiagonalization Methods"  \emph{SIAM J. Sci. Comput.} (2005).
#'
#' @seealso For other data reduction methods for \code{Spectra2D} objects, see
#' \code{\link{miaSpectra2D}} and \code{\link{pfacSpectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @export popSpectra2D
#'
#' @importFrom stats sd prcomp
#'
#' @examples
#' data(MUD1)
#' res <- popSpectra2D(MUD1)
#' plotScores(MUD1, res, main = "POP Scores", ellipse = "cls")
#' plotScree(res)
#' MUD1a <- plotLoadings2D(MUD1, res,
#'   load_lvls = c(-0.2, -0.1, 0.1, 0.2),
#'   load_cols = rep("black", 4), main = "POP Comp. 1 Loadings"
#' )
popSpectra2D <- function(spectra, n = 3, choice = "noscale", ...) {

  # Check everything first

  if (!requireNamespace("irlba", quietly = TRUE)) {
    stop("You need to install package irlba to use this function")
  }

  .chkArgs(mode = 21L)

  choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
  check <- choice %in% choices
  if (!check) stop("The choice of scaling parameter was invalid")

  chkSpectra(spectra)

  M <- .unstack(spectra)

  # Center & scale the data, then do PCA
  # irlba::prcomp_irlba does its own scaling, so we act accordingly

  if (choice == "noscale") {
    M <- scale(M, center = TRUE, scale = FALSE)
  }

  if (choice == "Pareto") {
    col.sd <- apply(M, 2, sd)
    M <- scale(M, center = TRUE, scale = col.sd^0.5)
  }

  if (choice == "autoscale") {
    col.sd <- apply(M, 2, sd)
    M <- scale(M, center = TRUE, scale = col.sd)
  }

  pca <- irlba::prcomp_irlba(x = M, n = n, center = FALSE, scale. = FALSE, ...)
  pca$method <- paste("centered/", choice, "/", "irlba", sep = "")
  class(pca) <- c("pop", "computed_via_irlba", class(pca))
  pca
}
