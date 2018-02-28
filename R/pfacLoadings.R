#'
#' Plot Loadings from a PARAFAC Analysis of a Spectra2D Object
#' 
#' Plots loadings from the PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' The loadings are computed by multipling matrix \code{A} by matrix \code{B}
#' in the \code{parafac} object, for a given component.  This matrix has dimensions
#' F2 x F1 and is a 2D pseudo-spectrum.  A reference spectrum may also be drawn.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param pfac An object of class \code{parafac} obtained by running \code{\link{pfacSpectra2D}}.
#'
#' @param load An integer specifying the loading to plot.
#'
#' @param ref An integer giving the spectrum in \code{spectra} to use
#'        as a reference spectrum, which is plotted behind the loadings.
#'        Defaults to \code{NULL} which does not plot a reference spectrum.
#'
#' @param ref_lvls A vector specifying the levels at which to compute contours
#'        for the reference spectrum.
#'        If \code{NULL}, values are computed using \code{calcLvls}.
#'
#' @param load_lvls A vector specifying the positive contour levels
#'        for the loadings pseudo-spectrum.
#'        If \code{NULL}, values are computed using \code{calcLvls}.
#'
#' @param ref_cols A vector specifying the colors for the contours in the reference
#'        spectrum. If \code{NULL}, set to gray.
#'
#' @param load_cols A vector specifying the colors for the contours in the laoding spectrum.
#'        If \code{NULL}, defaults to a scheme of nine values
#'        running from blue (low) to red (high), centered on green (zero).
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return The loadings matrix (pseudo-spectrum).  Side effect is a plot.
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
pfacLoadings <- function(spectra, pfac,
  load = 1, ref = NULL,
  load_lvls = NULL, ref_lvls = NULL,
  load_cols = NULL, ref_cols = NULL, ...) {
	
  if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
  if (class(pfac) != "parafac") stop("pfac argument was not a parafac object")
  if (length(load) != 1L) stop("Please supply a single loading")
  if (load > ncol(pfac$A)) stop("Requested component does not exist")
  if (!is.integer(ref)) stop("ref should be a single integer")
  if (length(ref) != 1L) stop("ref should be a single integer")
  chkSpectra2D(spectra)

  # Compute loading matrices
  M <- pfac$A[, load] %*% t(pfac$B[, load])
  #M <- t(M) # APPEARS TO BE NECESSARY TO ALIGN WITH REFERENCE SPECTRA
  M <- M[nrow(M):1,ncol(M):1]
  
  
  # Prep & send to plotEngine
  # .plotEngine expects a spectra object and lvls and cols as lists
  
  # Update spectra object
  ns <- length(spectra$names) # no of spectra
  spectra$data[[ns + 1]] <- t(M)
  spectra$names[ns + 1] <- "loadings"
  spectra$groups <- as.factor(c(spectra$groups, "loadings"))
  spectra$colors[ns + 1] <- "black"
  chkSpectra2D(spectra)
  
  # Configure levels
  
  if (is.null(ref)) { # only showing loadings
    if (is.null(load_lvls)) lvls <- NULL # .plotEngine will assign levels
    if (!is.null(load_lvls)) {
      lvls <- vector("list", 1)
      lvls[[1]] <- load_lvls
    }
  }
  
  if (!is.null(ref)) { # showing loadings and reference spectrum
    lvls <- vector("list", 2)  # intializes to NULL, NULL
    if (!is.null(ref_lvls)) lvls[[1]] <- ref_lvls
    if (is.null(ref_lvls)) lvls[[1]] <- NULL
    if (!is.null(load_lvls)) lvls[[2]] <- load_lvls	
    if (is.null(load_lvls)) lvls[[2]] <- NULL
    if ((is.null(load_lvls) & (is.null(ref_lvls)))) lvls <- NULL 
  }
    
  # Configure colors

  if (is.null(ref)) { # only showing loadings
    if (is.null(load_cols)) cols <- NULL # .plotEngine will assign colors
    if (!is.null(load_cols)) {
      cols <- vector("list", 1)
      cols[[1]] <- load_cols
    }
  }
  
  if (!is.null(ref)) { # showing loadings and reference spectrum
    cols <- vector("list", 2)  # intializes to NULL, NULL
    if (!is.null(ref_cols)) cols[[1]] <- ref_cols
    if (is.null(ref_cols)) cols[[1]] <- NULL
    if (!is.null(load_cols)) cols[[2]] <- load_cols	
    if (is.null(load_cols)) cols[[2]] <- NULL	
    if ((is.null(load_cols) & (is.null(ref_cols)))) cols <- NULL 
  }
  
  op <- par(no.readonly = TRUE) # save to restore later
  par(mai = c(1, 0.5, 1, 1))
  .plotEngine(spectra, which = c(ref, ns + 1), lvls = lvls, cols = cols, ...)
  on.exit(par(op)) # restore original values
    
  invisible(M)
}
