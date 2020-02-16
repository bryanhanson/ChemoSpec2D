#'
#' Draw a Histogram Showing Proposed Contour Levels
#'
#' @export
#' @noRd
#'

.sH <- function(M, lvs, ...) { # Helper function for showHist = TRUE

  # Check which arm is larger and use that for xlim
  ref <- .findExtreme(M)
  lim.x <- c(-ref, ref)

  def.par <- par(no.readonly = TRUE) # save to restore later (must call before layout)
  nf <- layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE), heights = c(6, 1))
  par(mar = c(3.1, 3.1, 1.1, 2.1))

  hist(M,
    breaks = 50,
    xlab = "", ylab = "", xlim = lim.x,
    col = "black", ...
  )
  abline(v = lvs, col = "pink", lty = 2)

  cscale <- .createScale()
  .drawScale(cscale, "horizontal")

  par(def.par)
} # end of .sH
