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

.plotEngine <- function(spectra, which = 1, lvls = NULL, cols = NULL, ...) {

  chkSpectra(spectra)
  
  # Plot each spectrum in turn
  for (i in 1:length(which)) {
  	
  	# cat("Plotting spectrum", i, "\n")
  	
    M <- spectra$data[[ which[i] ]]
    M <- t(M[nrow(M):1,]) # 90 cw prior to compensate for 90 ccw rotation built-in to contour
    
    # print(is.null(lvls[[i]]))
    # print(cols)
   
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

	  	  # print(curLvl)
	  	  # print(curCol)
	  	}
  	}
  	

  	if (i == 1) { # plot the first spectrum with decorations
  		
  	  # Handle user-provided xlim and/or ylim (rescale to [0...1] as required by contour)
	
	  # args <- as.list(match.call())[-1] # a COPY of the args for use with do.call
	  
	  # print(args)
	  
	  # if ("xlim" %in% names(args)) {
	  	# limx <- .rescale(eval(args$xlim), spectra$F2)
	  	# args$xlim <- NULL
	  	# args <- c(args, list(xlim = limx))
	  # }
	  
	  # if ("ylim" %in% names(args)) {
	  	# limy <- .rescale(eval(args$ylim), spectra$F1)
	  	# args$ylim <- NULL
	  	# args <- c(args, list(ylim = limy))
	  # }
	  
	  # clean up args (remove formals)
	  # args$spectra <- NULL
	  # args$which <- NULL
	  # args$lvls <- NULL
	  # args$cols <- NULL
	  
	  # args <- c(args, list(x = M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol))
	  
	  # print(args)
	  
  	  contour(x = spectra$F2, y = spectra$F1, M,
  	  	drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, ...)
  	  # do.call(contour, args)
  	  box()
  	  
      # Compute tick positions and labels, then draw
      
      F2ticks <- .computeTicks(spectra$F2)
      F2lab <- rev(formatC(F2ticks, digits = 2, format = "f"))
      F2at <- seq(min(spectra$F2), max(spectra$F2), length.out = length(F2lab))
            
      F1ticks <- .computeTicks(spectra$F1)
      F1lab <- rev(formatC(F1ticks, digits = 2, format = "f"))
      F1at <- seq(min(spectra$F1), max(spectra$F1), length.out = length(F1lab))
      
  	  axis(side = 1, at = F2at, labels = F2lab, cex.axis = 0.75)
  	  axis(side = 4, at = F1at, labels = F1lab, cex.axis = 0.75)
  	  
  	  mtext(spectra$unit[1], 1, line = 2)
  	  mtext(spectra$unit[2], 4, line = 2)
  	} # end of plotting first spectrum

    if (i > 1) { # Add any additional spectra requested
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, add = TRUE, ...)
    }
  } # end of master loop
} # end of .plotEngine
