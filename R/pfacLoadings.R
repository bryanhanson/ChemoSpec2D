#'
#' Plot Loadings from a PARAFAC Analysis of a Spectra2D Object
#' 
#' Plots loadings from the PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' The loadings are computed by multipling matrix \code{A} by matrix \code{B}
#' in the \code{parafac} object, for a given component.  This matrix has dimensions
#' F2 x F1 and is a 2D pseudo-spectrum.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param pfac An object of class \code{parafac} obtained by running \code{\link{pfacSpectra2D}}.
#'
#' @param which An integer specifying the loading to plot.
#'
#' @param ref An integer giving the spectrum in \code{spectra} to use
#'        as a reference spectrum, which is plotted behind the loadings.
#'        Defaults to \code{NULL} which does not plot a reference spectrum.
#'
#' @param rlvls A vector specifying the levels at which to compute contours
#'        for the reference spectrum.
#'        If \code{NULL}, values are computed using \code{guessLvls}.
#'
#' @param plvls A vector specifying the positive contour levels
#'        for the loadings pseudo-spectrum.
#'        If \code{NULL}, values are computed using \code{guessLvls}.
#'
#' @param nlvls A vector specifying the negative contour levels
#'        for the loadings pseudo-spectrum.
#'        If \code{NULL}, values are computed using \code{guessLvls}.
#'
#' @param colors A vector of 2-3 colors.  The first and second colors will be used for
#'        the positive and negative loading contours.  The third color will be used
#'        for the reference spectrum, if requested.
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
pfacLoadings <- function(spectra, pfac, which = 1, ref = NULL,
  plvls = NULL, nlvls = NULL, rlvls = NULL,
  colors = c("red", "blue", "grey"), ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  if (length(which) != 1L) stop("Please supply a single loading")
  if (which > ncol(pfac$A)) stop("Requested component does not exist")
  chkSpectra2D(spectra)

  # Helper function
  
  plotContours <- function(spectra, MP, MN, PLVLS, NLVLS, RLVLS, plvls, nlvls, rlvls, ref) {
  	# 6 combos must be considered!
  	if (PLVLS & NLVLS & RLVLS) { # plot all
  	  contour(x = spectra$F2, y = spectra$F1, z = spectra$data[[ref]], levels = rlvls,
  	    col = colors[3], drawlabels = FALSE, ...)
  	  contour(x = spectra$F2, y = spectra$F1, z = MP, levels = plvls,
  	    col = colors[1], drawlabels = FALSE, add = TRUE)  		
  	  contour(x = spectra$F2, y = spectra$F1, z = MN, levels = nlvls,
  	    col = colors[2], drawlabels = FALSE, add = TRUE)  		  		
  	}
  	
  	if (PLVLS & NLVLS & !RLVLS) { # plot + & -
  	  contour(x = spectra$F2, y = spectra$F1, z = MP, levels = plvls,
  	    col = colors[1], drawlabels = FALSE, ...)
  	  contour(x = spectra$F2, y = spectra$F1, z = MN, levels = nlvls,
  	    col = colors[2], drawlabels = FALSE, add = TRUE)  		
  	}

  	if (PLVLS & !NLVLS & RLVLS) { # plot + & ref
  	  contour(x = spectra$F2, y = spectra$F1, z = spectra$data[[ref]], levels = rlvls,
  	    col = colors[3], drawlabels = FALSE, ...)
  	  contour(x = spectra$F2, y = spectra$F1, z = MP, levels = plvls,
  	    col = colors[1], drawlabels = FALSE, add = TRUE)  		
  	}

  	if (!PLVLS & NLVLS & RLVLS) { # plot - & ref
  	  contour(x = spectra$F2, y = spectra$F1, z = spectra$data[[ref]], levels = rlvls,
  	    col = colors[3], drawlabels = FALSE, ...)
  	  contour(x = spectra$F2, y = spectra$F1, z = MN, levels = nlvls,
  	    col = colors[2], drawlabels = FALSE, add = TRUE)  		  			
  	}

  	if (PLVLS & !NLVLS & !RLVLS) { # plot + only
  	  contour(x = spectra$F2, y = spectra$F1, z = MP, levels = plvls,
  	    col = colors[1], drawlabels = FALSE, ...)  		
  	}
  	
  	if (!PLVLS & NLVLS & !RLVLS) { # plot - only
  	  contour(x = spectra$F2, y = spectra$F1, z = MN, levels = nlvls,
  	    col = colors[2], drawlabels = FALSE, ...)  		
  	}
  	
  } # end of helper function
  
  # Set up some flags to be certain we actually have levels to plot
  PLVLS <- FALSE
  NLVLS <- FALSE
  RLVLS <- FALSE
  if (!is.null(plvls)) PLVLS <- TRUE
  if (!is.null(nlvls)) NLVLS <- TRUE
  if (!is.null(rlvls)) RLVLS <- TRUE
  
  # Compute loading matrices
  M <- pfac$A[, which] %*% t(pfac$B[, which]) # all loadings
  MN <- M
  MN[MN > 0.0] <- 0.0 # negative loadings
  MP <- M
  MP[MP < 0.0] <- 0.0 # positive loadings
  
  # Compute levels for each loadings matrix (where not provided)
  if (is.null(plvls)) {
  	plvls <- guessLvls(MP)
  	if (length(plvls) > 0L) PLVLS <- TRUE
  }

  if (is.null(nlvls)) {
  	nlvls <- guessLvls(MN)
  	if (length(nlvls) > 0L) NLVLS <- TRUE
  }
  
  if (!is.null(ref)) {
  	if (length(colors) != 3L) stop("Please provide 3 colors for plotting")
    if (is.null(rlvls)) {
  	  rlvls <- guessLvls(spectra$data[[ref]])
  	  if (length(rlvls) > 0L) RLVLS <- TRUE
    }
  }
  # Plot
  plotContours(spectra, MP, MN, PLVLS, NLVLS, RLVLS, plvls, nlvls, rlvls, ref)
  
  invisible(M)
}
