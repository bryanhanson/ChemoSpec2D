#'
#' Compute Ticks for Contour Plots
#'
#' @param freq Numeric.  Vector of frequency values.
#'
#' @param n Integer.  The number of ticks desired. Defaults to 10, unless there are
#'        segments in which case roughly half the specified number.
#'
#' @export
#' @noRd
#'

.computeTicks <- function(freq, n = 10) {
    # Gaps in F1 or F2 may be present, complicating things
    # If no diff, then use seq(low, high, length.out = 10)
    # If diff, divide into sections, return 5 ticks per section
    
    discon <- FALSE # Check for discontinuities
    dfreq <- diff(freq)
    p <- dfreq[1]
    for (i in 1:length(dfreq)) {
    	if (!isTRUE(all.equal(dfreq[i], p))) {
    		discon <- TRUE
    		break
    	}
    }

    if (!discon) ticks <- pretty(freq, n)

    if (discon) { # there are discontinuities
    dm <- min(dfreq)
    # separate into sections, compute each separately & concatenate
    dc <- which(dfreq != dm)
    ticks <- pretty(freq[1]:freq[dc[1]], n = floor(n/2)) # first segment
    if (length(dc) >= 2) {
      for (i in 1:(length(dc)-1)) {
        # seg <- seq(freq[dc[i]+1], freq[dc[i + 1]], length.out = 5)
        seg <- pretty(freq[dc[i]+1]:freq[dc[i + 1]], n = floor(n/2))
        ticks <- c(ticks, seg) 
      }
    }
    ticks <- c(ticks, pretty(freq[dc[length(dc)] + 1]:freq[length(freq)], n = floor(n/2))) # last segment
  }
  return(ticks)
}
