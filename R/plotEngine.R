#'
#' Plotting Engine for 2D Spectra
#' 
#' Plots one or more 2D spectra stored in a \code{\link{Spectra2D}} object.
#' This is the function that actually creates the plots requested
#' by several other functions.  Not intended to be called by the user.
#' Base graphics functions are used.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer vector giving the spectra to be plotted.
#'        Spectra are plotted in order so the last one is on top.
#'
#' @param lvls A list of \code{length(which)}.  Each list element
#'        should be a numeric vector giving the desired contour levels.
#'        If any are \code{NULL}, values are computed using \code{chooseLvls}.
#'
#' @param colors A vector of colors of \code{length(which)}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @export
#'
#' @importFrom lattice contourplot lattice.getOption
#'
#' @examples
#'
#' data(MUD1)
#'
#' plotSpectra2D(MUD1)
#'
plotEngine <- function(spectra, which = 1, lvls = NULL, colors = NULL, ...) {
	
  if (missing(spectra)) stop("No spectral data provided")
  chkSpectra2D(spectra)
  if (!is.null(lvls)) { if (length(which) != length(lvls)) stop("length(which) != length(lvls)") }
  if (!is.null(colors)) { if (length(which) != length(colors)) stop("length(which) != length(colors)") }

  op <- par(no.readonly = TRUE) # save to restore later
  par(mai = c(1, 0.5, 1, 1))
  
  # Plot each spectrum in turn
  for (i in 1:length(which)) {
  	
    M <- spectra$data[[i]]
    M <- t(M[nrow(M):1,]) # 90 cw prior to compensate for 90 ccw rotation built-in to contour
    
  	if (is.null(lvls[[i]])) curLvl <- guessLvls(M)
  	if (!is.null(lvls[[i]])) curLvl <- lvls[[i]]
  	
  	if (is.null(colors)) curCol <- "black"
  	if (!is.null(colors)) curCol <- colors[[i]]

  	if (i == 1) { # plot the first spectrum with decorations
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, ...)
  	  box()
      # Compute tick positions and labels, then draw
      F2st <- spectra$F2[1]
      F2end <- spectra$F2[length(spectra$F2)]
      F2at <- seq(F2end, F2st, length.out = 10)
      F2lab <- as.character(round(F2at, 0))
      F2at <- seq(0.0, 1.0, length.out = 10)
      
      F1st <- spectra$F1[1]
      F1end <- spectra$F1[length(spectra$F1)]
      F1at <- seq(F1st, F1end, length.out = 10)
      F1lab <- as.character(round(F1at, 0))
      F1at <- seq(1.0, 0.0, length.out = 10)
     
  	  axis(side = 1, at = F2at, labels = F2lab)
  	  axis(side = 4, at = F1at, labels = F1lab)
  	  
  	  mtext(spectra$unit[1], 1, line = 2)
  	  mtext(spectra$unit[2], 4, line = 2)
  	}

    if (i > 1) {
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, add = TRUE, ...)
    }
  } # end of master loop
  
  on.exit(par(op)) # restore original values
}
