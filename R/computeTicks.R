
### computeTicks for contour plots

#'
#' @export
#' @noRd
#'

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

    if (!discon) ticks <- pretty(freq, n = 10)
    # if (!discon) ticks <- seq(min(freq), max(freq), length.out = 10)

    if (discon) { # there are discontinuities
    dm <- min(dfreq)
    # separate into sections, compute each separately & concatenate
    dc <- which(dfreq != dm)
    # ticks <- seq(freq[1], freq[dc[1]], length.out = 5) # first segment
    ticks <- pretty(freq[1]:freq[dc[1]], n = 5) # first segment
    if (length(dc) >= 2) {
      for (i in 1:(length(dc)-1)) {
        # seg <- seq(freq[dc[i]+1], freq[dc[i + 1]], length.out = 5)
        seg <- pretty(freq[dc[i]+1]:freq[dc[i + 1]], n = 5)
        ticks <- c(ticks, seg) 
      }
    }
    # ticks <- c(ticks, seq(freq[dc[length(dc)] + 1], freq[length(freq)], length.out = 5)) # last segment
    ticks <- c(ticks, pretty(freq[dc[length(dc)] + 1]:freq[length(freq)], n = 5)) # last segment
  }
  return(ticks)
}
