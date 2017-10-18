#'
#' Plot Loadings from a PARAFAC Analysis of a Spectra2D Object
#' 
#' Plots loadings from the PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' The loadings are computed by multipling matrix \code{A} by matrix \code{B}
#' in the \code{parafac} object, for a given component.  This matrix has dimensions
#' F2 x F1.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param pfac An object of class \code{parafac}.
#'
#' @param which An integer specifying the loading to plot.
#'
#' @param lvls An integer specifying the levels at which to compute contours.
#'        If \code{NULL}, values are computed using \code{chooseLvls}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @seealso Please see \code{\link{pfacSpectra2D}} for examples.
#' 
#' @export
#'
#' @importFrom graphics abline contour rect
#'
pfacLoadings <- function(spectra, pfac, which = 1, lvls = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Please supply a single loading")
  chkSpectra2D(spectra)

  M <- pfac$A[, which] %*% t(pfac$B[, which])
  
  if (is.null(lvls)) {
  	lvls <- chooseLvls(M, n = 10, mode = "poslog", lambda = 0.2)
  	lvls <- lvls[-1]
  }
  
  contour(x = spectra$F2, y = spectra$F1, z = M,
    levels = lvls, drawlabels = FALSE,...)
    
  # return(M)
}
