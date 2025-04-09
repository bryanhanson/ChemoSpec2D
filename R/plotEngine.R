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
#' @importFrom graphics axis box mtext abline contour text axTicks
#' @noRd
#'

.plotEngine <- function(spectra, which = 1, lvls = NULL, cols = NULL, showGrid = FALSE, ...) {
  chkSpectra(spectra)

  # Plot each spectrum in turn
  for (i in 1:length(which)) {
    M <- spectra$data[[which[i]]]
    M <- t(M[nrow(M):1, ]) # 90 cw prior to compensate for 90 ccw rotation built-in to contour

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
          "does not match \nthe number of levels provided (or automatically computed):",
          sep = " "
        )
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
      xlim2 <- range(spectra$F2)
      ylim2 <- range(spectra$F1)

      # Handle user-provided xlim and/or ylim, keeping in mind the different conventions
      # for 2D NMR plotting vs those of the contour function. xlim/ylim on [0...1]
      # AND origins as described just above.

      args <- as.list(match.call())[-1] # a COPY of the args for use with do.call

      if ("xlim" %in% names(args)) {
        xlim <- eval(args$xlim)
        xlim2 <- xlim
        args$xlim <- sort(.rescale(xlim, 1, 0, min(spectra$F2), max(spectra$F2)))
      }

      if ("ylim" %in% names(args)) {
        ylim <- eval(args$ylim)
        ylim2 <- ylim
        args$ylim <- sort(.rescale(ylim, 1, 0, min(spectra$F1), max(spectra$F1)))
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

      # Set up ticks

      F2ticks <- seq(xlim2[1], xlim2[2], length.out = length(axTicks(1)))
      F1ticks <- seq(ylim2[1], ylim2[2], length.out = length(axTicks(4)))

      # Format labels depending upon range
      integerLabThresh <- 10.0
      F2intLab <- F1intLab <- FALSE
      if (diff(range(xlim2)) > integerLabThresh) F2intLab <- TRUE
      if (diff(range(ylim2)) > integerLabThresh) F1intLab <- TRUE

      if (F2intLab) F2lab <- rev(formatC(F2ticks, digits = 0, format = "f"))
      if (!F2intLab) F2lab <- rev(formatC(F2ticks, digits = 2, format = "f"))

      if (F1intLab) F1lab <- rev(formatC(F1ticks, digits = 0, format = "f"))
      if (!F1intLab) F1lab <- rev(formatC(F1ticks, digits = 2, format = "f"))

      # Now draw ticks, xlab, ylab
      F2at <- axis(side = 1, at = axTicks(1), labels = F2lab, cex.axis = 0.75)
      F1at <- axis(side = 4, at = axTicks(4), labels = F1lab, cex.axis = 0.75)
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
