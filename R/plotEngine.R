#'
#' Plotting Engine for 2D Spectra
#' 
#' Plots one or more 2D spectra stored in a \code{\link{Spectra2D}} object.
#' This is the function that actually creates the plots requested
#' by several other functions.  Not intended to be called by the user.
#' Base graphics functions are used.  x and y axes dimensions are on [0...1]
#' par values should be adjusted before calling this function.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param which An integer vector giving the spectra to be plotted.
#'        Spectra are plotted in order so the last one requested is on top.
#'
#' @param lvls A list of \code{length(which)}.  Each list element
#'        should be a numeric vector giving the desired contour levels.
#'        If any are \code{NULL}, values are computed using \code{calcLvls}.
#'
#' @param cols A list of \code{length(which)}. Each list element
#'        should be a vector of valid color designations.  There should be
#'        one color per contour level.  Defaults to a scheme of nine values
#'        running from blue (low) to red (high), centered on green (zero).
#'
#' @param showGrid Logical. Shall a grid be drawn (corresponds to ticks).
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @importFrom graphics axis box mtext abline contour text
#' @export
#' @noRd
#'

.plotEngine <- function(spectra, which = 1, lvls = NULL, cols = NULL, showGrid = FALSE, ...) {

  chkSpectra(spectra)
  
  # Plot each spectrum in turn
  for (i in 1:length(which)) {
  	
    M <- spectra$data[[ which[i] ]]
    M <- t(M[nrow(M):1,]) # 90 cw prior to compensate for 90 ccw rotation built-in to contour
       
    if (is.null(lvls[[i]])) curLvl <- calcLvls(M, mode = "NMR")
  	if (!is.null(lvls[[i]])) curLvl <- lvls[[i]]
  	
  	# No. of lvls and cols must match, so if only one is passed the
  	# other must be made to match.  At this point we know the number
  	# of lvls regardless of how they were provided.  Fix cols accordingly.
  	
  	if (is.null(cols[[i]])) curCol <- .mapColors(spectra, curLvl)
  	if (!is.null(cols[[i]])) { # This is where a mismatch can occur
  		curCol <- cols[[i]]
	  	if (length(curLvl) != length(curCol)) {
	  	  msg <- paste("The number of colors provided for spectrum", which[i],
	  	    "does not match \nthe number of levels provided (or automatically computed):", sep = " ")
	  	  message(msg)
	  	  print(data.frame(noCols = length(cols), noLvls = length(lvls)))
	  	  message("Using automatic color assignment.  To avoid this, either provide \nboth lvls and cols or provide enough cols to match lvls in the table above")
	  	  curCol <- .mapColors(spectra, curLvl)
	  	}
  	}
  	

  	if (i == 1) { # plot the first spectrum with decorations
  		
	  # Keep in mind that the origin for the contour function has 0,0 at the lower left corner.
	  # However, the convention for 2D NMR has the origin for F2 on the right and the origin
	  # for F1 at the top. The actual matrix/spectrum has been rotated 90 CW to account
	  # for this convention (see above). We have to construct the axis labels manually due
	  # to the differing origin conventions mentioned just above.

      # Save the data ranges to assist with tick computations later
      # Initialize the requested range (to possibly be updated later)
      F2fullRange <- range(spectra$F2)
      F2reqRange <- range(spectra$F2)
      F1fullRange <- range(spectra$F1)
      F1reqRange <- range(spectra$F1)
      
  	  # Handle user-provided xlim and/or ylim, keeping in mind the different conventions
  	  # for 2D NMR plotting vs those of the contour function. xlim/ylim on [0...1]
	
	  args <- as.list(match.call())[-1] # a COPY of the args for use with do.call
	  
	  if ("xlim" %in% names(args)) {
	  	xlim <- eval(args$xlim)
	  	F2reqRange <- xlim
	  	limx <- sort(.rescale(xlim, spectra$F2, mode = 2L))
		limx <- limx + diff(limx) * 0.05 * c(-1.0, 1.0) # expand slightly to mimick usual base R behavior
	  	args$xlim <- NULL
	  	args <- c(args, list(xlim = limx))
	  }
	  
	  if ("ylim" %in% names(args)) {
	  	ylim <- eval(args$ylim)
	  	F1reqRange <- ylim
	  	limy <- sort(.rescale(ylim, spectra$F1, mode = 2L))
		limy <- limy + diff(limy) * 0.05 * c(-1.0, 1.0) # expand slightly to mimick usual base R behavior
	  	args$ylim <- NULL
	  	args <- c(args, list(ylim = limy))
	  }
	  
	  # clean up args (remove unneeded formals)
	  args$spectra <- NULL
	  args$which <- NULL
	  args$lvls <- NULL
	  args$cols <- NULL
	  args$grid <- NULL
	  args$showGrid <- NULL
	  
	  args <- c(args, list(x = M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol))
  	  do.call(contour, args)
  	  box()
  	  
      # Compute tick positions and labels, then draw them.
      # Final tick positions are on [0...1], the internal contour coordinates,
      # but .computeTicks works using native coords of the data
      # .computeTicks also takes into account any gaps
      #
      # Two interacting issues to consider here:
      # 1. For a large ppm range nucleus like 13C we are going to use integer labels and space ticks more widely.
      #    The opposite for 1H and similar nuclei.
      # 2. If xlim or ylim is specified and relatively narrow, we need to increase the number of ticks
      #    because otherwise one gets oddly spaced ticks and the system appears to not respect the
      #    requested limits.
      #
      # Figure out what fraction of the entire plot range has been requested, and make sure that the requested
      # range has 10 ticks (if the requested range is small, the full range will have a lot of ticks, but these
      # will not be drawn).
      #
      
      # Compute fraction of data to be shown (conveniently, we can use .rescale for this)
      fracF2 <- diff(.rescale(F2reqRange, spectra$F2))
      fracF1 <- diff(.rescale(F1reqRange, spectra$F1))
      
      # Compute number of ticks to request (10 is the minimum, but see .computeTicks re: discontinuous data)
      noTicksF2 <- floor(10/fracF2)
      noTicksF1 <- floor(10/fracF1)
      
      # Compute tick strings and positions
      F2ticks <- .computeTicks(spectra$F2, noTicksF2) # native coordinates
      F2at <- .rescale(F2ticks, spectra$F2, mode = 1L) # internal coordinates [0...1]
      F1ticks <- .computeTicks(spectra$F1, noTicksF1)
      F1at <- .rescale(F1ticks, spectra$F1, mode = 1L)     
     
      # Format labels depending upon range
      integerLabThresh <- 10.0
      F2intLab <- F1intLab <- FALSE
      if (diff(range(F2reqRange)) > integerLabThresh) F2intLab <- TRUE
      if (diff(range(F1reqRange)) > integerLabThresh) F1intLab <- TRUE

      if (F2intLab) F2lab <- rev(formatC(F2ticks, digits = 0, format = "f"))
      if (!F2intLab) F2lab <- rev(formatC(F2ticks, digits = 2, format = "f"))
      
      if (F1intLab) F1lab <- rev(formatC(F1ticks, digits = 0, format = "f"))
      if (!F1intLab) F1lab <- rev(formatC(F1ticks, digits = 2, format = "f"))
      
      # Now draw ticks, xlab, ylab
  	  axis(side = 1, at = F2at, labels = F2lab, cex.axis = 0.75)
  	  axis(side = 4, at = F1at, labels = F1lab, cex.axis = 0.75)
  	  mtext(spectra$unit[1], 1, line = 3)
  	  mtext(spectra$unit[2], 4, line = 3)
        	  
  	} # end of plotting first spectrum

    if (i > 1) { # Add any additional spectra requested
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, add = TRUE, ...)
    }
  } # end of master loop
  
  if (showGrid) {
    abline(v = F2at, col = "gray", lty = "dotted")
    abline(h = F1at, col = "gray", lty = "dotted")
  }
 
} # end of .plotEngine
