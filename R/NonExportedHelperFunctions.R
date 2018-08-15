#
# These are non-exported helper functions
#
# They are very lean and don't necessarily check arguments, so the calling
# function must check the arguments.
#
# When things are documented roxygen2 style, use @noRd to prevent an entry in the manual
#

##### Functions Related to Contour Plots #####

### plotEngine

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
#' @importFrom graphics axis box mtext
#' @noRd
#'
.plotEngine <- function(spectra, which = 1, lvls = NULL, cols = NULL, ...) {


  if (missing(spectra)) stop("No spectral data provided")
  chkSpectra2D(spectra)
  
  # Plot each spectrum in turn
  for (i in 1:length(which)) {
  	
    M <- spectra$data[[ which[i] ]]
    M <- t(M[nrow(M):1,]) # 90 cw prior to compensate for 90 ccw rotation built-in to contour
    
  	if (is.null(lvls[[i]])) curLvl <- calcLvls(M, mode = "NMR")
  	if (!is.null(lvls[[i]])) curLvl <- lvls[[i]]
  	
  	if (is.null(cols[[i]])) curCol <- .mapColors(spectra, curLvl)
  	if (!is.null(cols[[i]])) curCol <- cols[[i]]
  	
  	if (length(curLvl) != length(curCol)) {
  	  msg <- paste("The number of colors provided for spectrum", which[i],
  	    "does not match the number of levels provided:", sep = " ")
  	  message(msg)
  	  print(data.frame(noCols = length(cols), noLvls = length(lvls)))
  	  stop("See above and revise accordingly")
  	}

  	if (i == 1) { # plot the first spectrum with decorations
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, ...)
  	  box()
  	  
      # Compute tick positions and labels, then draw
      
      F2ticks <- .computeTicks(spectra$F2)
      F2lab <- rev(formatC(F2ticks, digits = 2, format = "f"))
      F2at <- seq(0.0, 1.0, length.out = length(F2lab))
            
      F1ticks <- .computeTicks(spectra$F1)
      F1lab <- formatC(F1ticks, digits = 2, format = "f")
      F1at <- seq(1.0, 0.0, length.out = length(F1lab))
      
  	  axis(side = 1, at = F2at, labels = F2lab, cex.axis = 0.75)
  	  axis(side = 4, at = F1at, labels = F1lab, cex.axis = 0.75)
  	  
  	  mtext(spectra$unit[1], 1, line = 2)
  	  mtext(spectra$unit[2], 4, line = 2)
  	} # end of plotting first spectrum

    if (i > 1) {
  	  contour(M, drawlabels = FALSE, axes = FALSE, levels = curLvl, col = curCol, add = TRUE, ...)
    }
  } # end of master loop
} # end of .plotEngine

### Map colors to go with each contour level

# This function should accept arbitrary vectors of levels and map them
# onto the reference color scale.  The color map should maintain symmetry present
# in the levels so -4.1 should give a color symmetric to +4.1
# Mapping should be relative to the range of the entire Spectra2D object, not just
# a single spectrum.  That way different spectra can be compared directly.

.mapColors <- function(spectra, lvls) {

  cscale <- .createScale()
  drange <- range(spectra$data, na.rm = TRUE) # some data sets have NAs
  drange <- .symAroundZero(drange)
  refscale <- seq(drange[1], drange[2], length.out = 10) # Not 9, surprisingly (need 9 intervals)
  myc <- cscale[findInterval(lvls, refscale, all.inside = TRUE)]
  return(myc)
}

### Adjust values to be symmetric around zero so that scale is applied correctly
# Find the most extreme of two values, and return it and its negative

.symAroundZero <- function(x) {
	
  if (length(x) != 2L) stop(".symAroundZero did not get two values")
		
  # Check for pos and neg values
  P <- N <- FALSE # flags for the existence of pos and/or neg values
  pos <- x[x > 0]
  if (length(pos) > 0) P <- TRUE
  neg <- x[x < 0]
  if (length(neg) > 0) N <- TRUE

  if ((P) & (!N)) vals <- c(-max(x), max(x))
  if ((!P) & (N)) vals <- c(min(x), -min(x))
  if ((P) & (N)) {
    mep <- max(pos) # most extreme pos value
    men <- min(neg) # most extreme neg value
    if (mep >= abs(men))  vals <- c(-mep, mep)	
    if (abs(men) >= mep) vals <- c(men, -men)
    }
	
	return(vals)
}

### Construct default color scale

.createScale <- function() {
  # blue/low -> red/high, anchored at zero (index 5, a shade of green)
  # view with:
  # pie(rep(1, 9), col = cscale)
	col1 <- rev(rainbow(5, start = 0.0, end = 0.25))
	col2 <- rev(rainbow(4, start = 0.45, end = 0.66))
	cscale <- c(col2, col1)	
}

### Draw the scale/legend

.drawScale <- function(cscale, orient) { # Draw a scale for reference

	nc <- length(cscale)
	
	if (orient == "horizontal") {
	  plot(1:nc, rep(1.0, nc), type = "n",
		yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
	  for (i in 1:nc) {
		rect(i-0.5, 0.5, i+0.5, 1.5, border = NA, col = cscale[i])
	  }	
	} # end of orient == "horizontal"

	if (orient == "vertical") {
      plot.new()
      op <- par(no.readonly = TRUE) # save to restore later (must call before layout)
      par(mai = c(0.5, 3.4, 0.5, 3.4))

	  plot(rep(1.0, nc), 1:nc, type = "n",
		yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
	  for (i in 1:nc) {
		rect(0.5, i-0.5, 1.5, i+0.5, border = NA, col = cscale[i])
	  }
	  text(0.0, 0.5, labels = "test") # not working
      on.exit(par(op)) # restore original values

	} # end of orient == "vertical"
	
}

### computeTicks for contour plots

.computeTicks <- function(freq) {
    # Gaps in F1 or F2 may be present, complicating things
    # If no diff, then use seq(low, high, length.out = 10)
    # If diff, divide into sections, return 5 ticks per section
    
    discon <- FALSE # Check for discontinuities
    dfreq <- diff(freq)
    p <- min(dfreq)
    for (i in 1:length(dfreq)) {
    	if (!isTRUE(all.equal(dfreq[i], p, scale = 1.0))) {
    		discon <- TRUE
    		break
    	}
    }

    if (!discon) ticks <- seq(min(freq), max(freq), length.out = 10)

    if (discon) { # there are discontinuities
    dm <- min(dfreq)
    # separate into sections, compute each separately & concatenate
    dc <- which(dfreq != dm)
    ticks <- seq(freq[1], freq[dc[1]], length.out = 5) # first segment
    if (length(dc) >= 2) {
      for (i in 1:(length(dc)-1)) {
        seg <- seq(freq[dc[i]+1], freq[dc[i + 1]], length.out = 5)
        ticks <- c(ticks, seg) 
      }
    }
    ticks <- c(ticks, seq(freq[dc[length(dc)] + 1], freq[length(freq)], length.out = 5)) # last segment
  }
  return(ticks)
}

##### Misc Utility Functions #####

### Get limits from a user specified formula (used in removeFreq2D and removePeaks2D)
	
# The user may not know or think about whether F2 or F1 is ascending or descending
# so we will try to get it right no matter how the user gives
# the formula; e.g. 6 ~ 3 ought to be handled as 3 ~ 6.
	
.getLimits <- function(spectra, dim, form) {
	lhs <- form[[2]]
	rhs <- form[[3]]
	if (as.character(lhs) == "low") lhs <- min(spectra[[dim]])
	if (as.character(lhs) == "high") lhs <- max(spectra[[dim]]) 
	if (as.character(rhs) == "low") rhs <- min(spectra[[dim]])
	if (as.character(rhs) == "high") rhs <- max(spectra[[dim]])
	ans <- c(lhs, rhs)
	if (is.unsorted(ans)) ans <- rev(ans)
	return(ans) # should always give numeric values in order
}


### findNA

#'
#'
#' Find NA in a Spectra2D Object
#' 
#' This function identifies the extent of any NA in a \code{\link{Spectra2D}} object.
#' This information can be used for summaries and plots.  Not intended to be called
#' by the user.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param retFreq Logical. Should the frequencies be returned?
#'
#' @return A list with two elements giving the indices of NAs for rows and columns,
#'         unless \code{retFreq = TRUE}, in which case the frequencies are returned.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#' @noRd
#'
.findNA <- function(spectra, retFreq = FALSE) {

	if (missing(spectra)) stop("No spectral data provided")
	chkSpectra2D(spectra)
	
	M <- spectra$data[[1]] # All spectra are assumed to have the same set of NAs
	                       # This is verified by chkSpectra2D
	
	# Find rows (columns) that are all NA
	
	# Check along the rows (F2, but displayed as vertical lines in plotSpectra2D)
	rNA <- rep(NA_integer_, ncol(M))
	for (i in 1:ncol(M)) {
		if (all(is.na(M[,i]))) rNA[i] <- i
	}
	rNA <- as.integer(na.omit(rNA))
	
	# For rows, we are checking each column left-to-right corresponding to the column
	# indices of M. As spectra$F2 is sorted ascending, but plotted descending, 
	# we must reverse F2 if we want correct frequencies for plotting.
	# No such fix is needed for the columns.
    if (retFreq) rNA <- rev(spectra$F2)[rNA]
    
	# Check along the columns (F1, but displayed as horizontal lines in plotSpectra2D)
	cNA <- rep(NA_integer_, nrow(M))
	for (i in 1:nrow(M)) {
		if (all(is.na(M[i,]))) cNA[i] <- i
	}
	cNA <- as.integer(na.omit(cNA))
    if (retFreq) cNA <- spectra$F1[cNA]
	
	return(list(rowNA = rNA, colNA = cNA))
	}


### check4Gaps

#'
#' Check for Discontinuities (Gaps) in a Vector
#' 
#' The basic procedure is to compare x[n + 1] - x[n] for successive values of
#' n.  When this value jumps, there is a gap which is flagged. \code{beg.indx}
#' and \code{end.indx} will always be contiguous as indices must be; it is the
#' \code{x} values that jump or have the gap.  The indices are provided as they
#' are more convenient in some programming contexts.  If not assigned, the
#' result appears at the console.
#' 
#' @param x A numeric vector to be checked for gaps.
#' 
#' 
#' @param tol A number indicating the tolerance for checking to see if the step
#' between successive \code{x} values are the same.  Depending upon how the
#' \code{x} values are stored and rounded, you may need to change the value of
#' \code{tol} to avoid flagging trivial "gaps".
#' 
#' @return A data frame giving the data chunks found, with one chunk per line.
#' \item{beg.freq }{The first frequency value in a
#' given data chunk.} \item{end.freq }{The last frequency value in a given data
#' chunk.} \item{size }{The length (in frequency units) of the data chunk.}
#' \item{beg.indx }{The index of the first frequency value in the data chunk.}
#' \item{eng.indx }{The index of the last frequency value in the data chunk.}
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @keywords utilities
#' @noRd
#'
.check4Gaps <- function(x, tol = 0.01) {
	
# Code derived from ChemoSpec::check4Gaps

	len.x <- length(x)
	xdiff <- abs(diff(x))
	p <- min(xdiff) # nominal freq/pt
	d1 <- x[1] # beg of data chunk by value
	d1i <- 1L # beg of data chunk by index
	d2 <- c() # end of data chunk by value
	d2i <- c() # end of data chunk by index

	# Check for gaps and build up values and indices
	for (i in 1:length(xdiff)) {
		# Nuance of all.equal pointed out by Dana Nadler, e-mails March 2017. Thanks!
		if (!isTRUE(all.equal(xdiff[i], p, tolerance = tol, scale = 1.0))) { # detects discontinuity
			d1 <- c(d1, x[i+1])
			d1i <- c(d1i, i+1)
			d2 <- c(d2, x[i])
			d2i <- c(d2i, i)
			}	
		}
	# Add the last entry
	d2 <- c(d2, x[len.x])
	d2i <- c(d2i, len.x)

	DF <- data.frame(beg.freq = d1, end.freq = d2, size = NA, beg.indx = d1i, end.indx = d2i)
	DF$size <- DF$end.freq - DF$beg.freq
	
	return(DF)
	}

### Helper Functions related to calcLvls

.sH <- function(M, lvs, ...) { # Helper function for showHist = TRUE
	
	# Check which arm is larger and use that for xlim
	ref <- .findExtreme(M)
	lim.x <- c(-ref, ref)

	def.par <- par(no.readonly = TRUE) # save to restore later (must call before layout) 
	nf <- layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), heights = c(6 , 1))
	par(mar = c(3.1, 3.1, 1.1, 2.1))

	hist(M, breaks = 50,
		xlab = "", ylab = "", xlim = lim.x,
		col = "black", ...)
	abline(v = lvs, col = "pink", lty = 2)
	
	cscale <- .createScale()
	.drawScale(cscale, "horizontal")
		
	par(def.par)
} # end of .sH


.findExtreme <- function(M) {
	# Find the most extreme value in a numerical object (matrix)
	# and return the absolute value of that extreme
	# Must handle NAs
	M <- M[!is.na(M)]
	ex <- abs(range(M))
	ex <- ex[which.max(ex)]
	return(ex)
}
		
.getPN <- function(M) {
	# Get either the (+)-ive or (-)-ive values in a matrix
	# depending upon which is most extreme in absolute terms,
	# and return them as a vector of positive values
	
	# Be sure to weed out NA's
	
	neg <- M[M < 0] # these are vectors
	neg <- neg[!is.na(neg)]
	pos <- M[M > 0]
	pos <- pos[!is.na(pos)]
	if (length(pos) == 0) return(neg) # no + values
	if (length(neg) == 0) return(pos) # no - values
	exP <- .findExtreme(pos)		
	exN <- .findExtreme(neg)
	if (exP >= exN) return(pos)
	if (exN > exP) return(abs(neg))
}
			
### extraData

# Argument 'indices' is used by removeSample2D and removeGroup2D to report
# the indices of the samples that were removed, so the user can use them
# to manually fix the extra data.

.extraData <- function(spectra, indices) {
	trouble <- FALSE
	spec.names <- names(spectra)
	reqd.names <- c("F2", "F1", "data", "names", "groups", "colors", "units", "desc")
	extra <- setdiff(spec.names, reqd.names)
	
	if (length(extra) > 0) {
		
		ns <- length(spectra$names)

		# Give the extra data names & check their lengths
		for (i in 1:length(extra)) {
			msg <- paste("\tAdditional data was found:", extra[i], sep = " ")
			message(msg)		
			if (length(spectra[[extra[i]]]) != ns) {
				msg <- paste("\tThe length of *", extra[i],
					"* did not match the number of samples.\n", sep = "")
				message(msg)
				trouble <- TRUE			
			}
		}
		
		# If something was removed, give the indices		
		if (!missing(indices)) {
			message("\tIf these are per sample data, you may have to manually edit them.")
			message("\tThe removal indices are:")
			print(indices)		
		}
		
	}
	
	return(trouble)
}


