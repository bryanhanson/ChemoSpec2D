#'
#' Globally Align 2D NMR Spectra, Including Stacks of 2D Spectra
#'
#' This function attempts to align (multiple) 2D NMR spectra via a simple optimization
#' approach, on a discrete grid corresponding to the matrix indices.
#' The potential alignment space runs from \code{-maxColShift} \ldots \code{maxColShift} and \code{-maxRowShift}
#' \ldots \code{maxRowShift}. The mask matrix is moved over the reference matrix starting at the center,
#' and the region of overlap is evaluated by the objective function.  Think of this as moving the
#' the center of the mask over the reference matrix while looking for the best fit. 
#'
#' The strategy used does not involve any random moves.  The search for the optimum alignment starts
#' by checking the situation with no shifts.  If the threshold is exceeded the search is done.  If the threshold
#' is not exceeded, a coordinate ascent strategy is used to find the next position to evaluate. This is repeated
#' until one of the stopping criteria are satisfied.
#'
#' @param Ref Numeric Array. An array of 2D spectra with the first dimension being \code{1:no. samples}.
#'        A single spectrum is fine as along as it is an array, with first dimension = 1.
#'
#' @param Mask Numeric Array. As for \code{Ref} but will serve as the mask.
#'
#' @param maxColShift Integer.  The most extreme positive x-value to allow when moving the mask
#'        relative to the reference.  Put another way, the largest shift in the columns
#'        that will be allowed.
#'
#' @param maxRowShift Integer.  As for \code{maxColShift}, but for the y-dimension/rows.
#'
#' @param thres Numeric. The stopping criterion. Once the objective function reaches
#'        this value the search stops.  The objective function is the cosine of the angle
#'        between the unstacked spectra, so \code{thres} should be on [0 \ldots 1].
#'
#' @param iter Integer. The maximum number of iterations.
#'
#' @param title Character. An optional title string to go on the diagnostic plots.
#'
#' @param stopWhen Length two numeric vector.  The first value is a change in the
#'        objective function. Let's call it \code{v}.  The second value is a number of data points
#'        (let's call it \code{n}, coerced to integer). The search process is stopped when the average
#'        *change* of the most recent \code{n} values of the objective function falls below \code{v}.
#'
#' @param plot Logical. Shall a plot of the alignment progress be made?  The plot is useful for
#'        diagnostic purposes.
#'
#' @param debug Integer. \code{debug = 1} applies to the calling function and simply reports
#'        on the progress of alignment.  \code{debug = 2} reports on the outcome of the
#'        alignment process.  However, the same information and more is displayed in the
#'        diagnostic plots.
#'
#' @return A list, with two elements: \code{AA} the aligned matrix, which is a shifted version
#'         of the mask (Mask), and \code{diagnostics}, a data frame providing information
#'         about the history of the alignment (and is the basis for the diagnostic plot).
#'
#' @references
#' Roughly follows the algorithm described in Robinette et al. 2011
#'            \emph{Anal. Chem.} vol. 83, 1649-1657 (2011) dx.doi.org/10.1021/ac102724x
#'
#' @importFrom graphics legend points
#' @importFrom stats na.omit optimize
#'
#' @export
#' @noRd
#'

.globalAlignArraysCA <- function(Ref, Mask,
	maxColShift = 40, maxRowShift = 40,
	thres = 0.98, iter = 20,  stopWhen = c(0.02, 5),
	plot = FALSE, title = NULL, debug = 0L) {

  if (!all(dim(Ref)[2:3] == dim(Mask)[2:3])) stop("Arrays Ref and Mask must have the same dimensions")
  
  # The method here based on bookdown.org/rdpeng/advstatcomp/coordinate-descent.html
  # Step 0. Initialize a Few Things.
    
  currOF <- 0.0 # OF is 1 - distance so we want to MAXimize this value
  posX <- 0L
  posY <- 0L
  outcome <- "Exceeded threshold"
  
  historyX <- rep(NA_integer_, iter)
  historyY <- rep(NA_integer_, iter)
  historyOF <- rep(NA_real_, iter)
  
  # Helper Functions
  fx <- function(x, y, Mask, Ref) {
  	.evalMatrixOverlap(Ref, Mask, x, y)
  }
  
  fy <- function(y, x, Mask, Ref) {
  	.evalMatrixOverlap(Ref, Mask, x, y)
  }
 
  # Check to see, if by chance, the matrices are perfectly aligned with no shifts.
  # If so, the while loop will be skipped saving lots of time.

  currOF <- .evalMatrixOverlap(Ref, Mask, 0, 0) # iteration 1
  historyX[1] <- 0L
  historyY[1] <- 0L
  historyOF[1] <- currOF
  
  it <- 2L # iteration counter

  # Step 1. Search for the optimum
  
  while (currOF < thres) {
    
    # Each time we enter this loop we have fresh values of posX and posY
        
    if (it > stopWhen[2]) {
    	  if (mean(diff(na.omit(historyOF[(it-stopWhen[2]):it]))) < stopWhen[1]) {
          if (debug >= 2) message("stopWhen criteria met, stopping")
          outcome <- "stopWhen criteria met"
          break
      }
    }

    if (it > iter) {
      if (debug >= 2) message("Maximum iterations reached, stopping")
      outcome <- "Maximum iterations reached"
      break
    }
    
    if ((it %% 2) == 0) {
      opX <- optimize(fx, c(-1*maxColShift, maxColShift), y = posY, Mask = Mask, Ref = Ref, maximum = TRUE)
      posX <- round(opX$maximum) # must round here, as will be coerced later when used as index
      currOF <- opX$objective
    }
    
    if ((it %% 2) == 1) {
      opY <- optimize(fy, c(-1*maxColShift, maxColShift), x = posX, Mask = Mask, Ref = Ref, maximum = TRUE)
      posY <- round(opY$maximum)
      currOF <- opY$objective
    }
        
    # update history
    historyX[it] <- posX
    historyY[it] <- posY
    historyOF[it] <- currOF
        
    it <- it + 1
  } # End of while "loop" -- optimal answers in current values
  
  # Step 2. Prepare diagnostic info and optionally, make the plot
  
  DF <- data.frame(x = historyX,
  	               y = historyY,
  	               OF = historyOF)
  DF <- DF[1:(it-1),]
  best <- which.max(DF$OF)
  bestX <- DF$x[best]
  bestY <- DF$y[best]
            
  if (plot) {
  	op <- par(no.readonly = TRUE)
   	par(mfrow = c(2,1))
   	
  	# first plot
  	plot(DF$OF, type = "b", xlab = "iteration", ylab = "OF", ylim = c(0, 1))
  	abline(h = thres, col = "gray90")
  	points(nrow(DF), DF$OF[best], pch = 20)
  	mtext("Alignment Progress", line = 1)
  	mtext("black dot = optimal alignment", line = 0, col = "black", cex = 0.75)
   	legend("bottomleft", outcome, col = "black", cex = 0.75, bty = "n")
    info <- paste("max. OF = ", round(DF$OF[best], 3), sep = " ")
  	mtext(info, side = 4)
 	
  	# informative title
  	if (!is.null(title)) {
   	  tcex <- 1.3
  	  if (nchar(title) > 60) tcex = 1.1
  	  if (nchar(title) > 80) tcex = 0.9
   	  if (nchar(title) > 100) tcex = 0.7
 	  mtext(title, side = 1, line = 5.75, col = "blue", cex = tcex)	
  	}
  	
  	# second plot
  	dx <- range(DF$x) # expand limits a bit
  	dx <- dx + 0.5 * diff(dx) * c(-1, 1)
  	dy <- range(DF$y)
  	dy <- dy + 0.5 * diff(dy) * c(-1, 1)
  	plot(DF$x, DF$y, type = "b",
  	  xlim = dx, ylim = dy,
  	  xlab = "x position", ylab = "y position")
  	abline(h = 0, v = 0, col = "gray90")
  	points(DF$x[best], DF$y[best], pch = 20)
  	mtext("Search Path", line = 1)
  	mtext("black dot = optimal alignment", line = 0, col = "black", cex = 0.75)
  	info <- paste("opt. shift: x =", round(DF$x[best]), "y =", round(DF$y[best]), sep = " ")
  	mtext(info, side = 4)
  	on.exit(par(op))
  } # end of plot == TRUE

  # Step 3. Create the modified mask
  # The mask (Mask) is "moved", not the reference
  # If shift is zero simply return the mask

  # optimal alignment is no change -- exit now
  if ((posX == 0L) & (posY == 0L)) return(list(AA = Mask, diagnostics = DF))

  # otherwise shift
  
  Ash <- .shiftArray(Mask, -bestX, -bestY, fill = "zero")
  dimnames(Ash) <- NULL
  return(list(AA = Ash, diagnostics = DF))
}

