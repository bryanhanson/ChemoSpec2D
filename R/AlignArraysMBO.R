#'
#' Globally Align Arrays Composed of Stacks of 2D NMR Spectra
#'
#' This function aligns multiple 2D NMR spectra, working with the spectra internally as arrays.
#' Alignment occurs on a discrete grid corresponding to the arrary indices. The arrays are arranged
#' such that one is aligning their projection onto a 2D grid, which means one is shifting the
#' arrays in 2D.  If you were doing this visually with transparencies of two 2D spectra, one the
#' reference and one the mask, you would shift the transparencies around to get the best visual alignment,
#' but the only possible answers require alignment at particular data pointa/indices.
#' The potential alignment space runs from \code{-maxColShift} \ldots \code{maxColShift} and \code{-maxRowShift}
#' \ldots \code{maxRowShift}.
#'
#' The strategy used is "Model Based Optimization" or MBO, which is also known as "Bayesian Optimization"
#' or BO.  Here, the search for the optimum alignment starts by checking if no shift exceeds the threshold.
#' If so, no search is needed and time is saved.  If the threshold is not exceeded, we proceed to MBO/BO.
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
#' @param thres Numeric. Prior to launching the optimization, the objective function is evaluated
#'        for no shift in case this is actually the best alignment (saving a great deal of time).
#'        If this initial check exceeds the value of \code{thres}, no optimization is performed
#'        and the unshifted spectra are returned.  The objective function is the cosine of the angle
#'        between the unstacked spectra, so \code{thres} should be on [0 \ldots 1].
#'
#' @param no.it Integer. The maximum number of iterations in the optimization.
#'
#' @param restarts Integer. The maximum number of independent rounds of optimization.
#'
#' @param NS Array. An array of noise surfaces produced by \code{.noiseSurface}.
#'
#' @param debug Integer.  Values >= 1 give messages about alignment progress in black text.  Values >= 2
#'        print the merge matrix from the \code{hclust} object, which serves as the guide tree.
#'        Depending upon the optimization method values >= 2 may give additional information.
#'        For \code{method = "MBO"} values less than 2 suppress some messages and warnings from
#'        the underlying functions.  If the alignment doesn't work well, set \code{debug = 2}.
#'        Setting \code{plot = TRUE} also gives a view of alignment progress.
#'
#' @param dist_method Character. The distance method to use in the objective function.
#'        See \code{\link{rowDist}} for options.  You may wish to use \code{\link{sampleDist}}
#'        to visualize the distances ahead of time.  If a particular method doesn't distinguish
#'        between samples it won't give a good alignment result.
#'
#' @param plot Logical. Shall a plot of the alignment progress be made?  The plot is useful for
#'        diagnostic purposes.  Every step of the alignment has a corresponding plot so you should
#'        probably direct the output to a pdf file.
#'
#' @param minimize Logical. Is the goal to minimize the objective function?  If so, use \code{TRUE}.
#'
#' @param fill Aligning spectra requires that at least some spectra be shifted left/right
#'        and up/down.  When a spectrum is shifted, spaces are opened that must be filled with something:
#'  \itemize{
#'    \item If \code{fill = "zeros"} the spaces are filled with zeros.
#'    \item If \code{fill = "noise"} the spaces are filled with an estimate of the noise from the
#'      original spectrum.
#'  }
#'
#' @return A list with two elements: 1. The aligned matrix, which is a shifted version of the mask (AA).
#'         2. A length two integer vector containing the optimal x and y shifts (shift).
#'
#' @export
#' @noRd
#'

.AlignArraysMBO <- function(Ref, Mask,
                            maxColShift = 40, maxRowShift = 40,
                            dist_method = "cosine", minimize = FALSE, thres = 0.99,
                            no.it = 20L, restarts = 2L, fill = "noise", NS = NULL,
                            plot = FALSE, debug = 1) {
  if (!all(dim(Ref)[2:3] == dim(Mask)[2:3])) stop("Arrays Ref and Mask must have the same dimensions")

  # Step 1. Check to see, if by chance, the matrices are perfectly aligned with no shifts.
  # If so, we can skip the optimization process, and exit now.

  currOF <- .evalArrayOverlapMBO(c(0L, 0L), dist_method)

  if (minimize) {
    if (currOF <= thres) {
      if (debug >= 1) cat("[ChemoSpec2D] No alignment necessary: objective function value is below threshold\n\n")
      return(list(AA = Mask, shift = c(0L, 0L)))
    }
  }

  if (!minimize) {
    if (currOF >= thres) {
      if (debug >= 1) cat("[ChemoSpec2D] No alignment necessary: objective function value exceeds threshold\n\n")
      return(list(AA = Mask, shift = c(0L, 0L)))
    }
  }

  # Step 2.  Run MBO to find best overlap

  makeF <- function(Ref, Mask) {
    force(Ref)
    force(Mask)
    # see .evalArrayOverlapMBO for the mechanism of retrieving Ref and Mask from the calling environment
    function(x) {
      .evalArrayOverlapMBO(x, dist_method)
    }
  }

  optFun <- makeF(Ref, Mask)

  objF <- smoof::makeSingleObjectiveFunction(
    name = "Eval Array Overlap MBO",
    fn = optFun,
    minimize = minimize,
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerVectorParam(
        len = 2L,
        id = "x",
        lower = c(-maxColShift, -maxRowShift),
        upper = c(maxColShift, maxRowShift)
      )
    )
  )

  search_space <- (2 * maxColShift + 1) * (2 * maxRowShift + 1)

  des <- ParamHelpers::generateDesign(n = 20L, ParamHelpers::getParamSet(objF), fun = lhs::randomLHS)
  surrogate <- mlr::makeLearner("regr.km",
    predict.type = "se",
    covtype = "matern3_2"
  )
  ctrl <- mlrMBO::makeMBOControl()
  ctrl <- mlrMBO::setMBOControlTermination(ctrl, iters = no.it)
  ctrl <- mlrMBO::setMBOControlInfill(ctrl,
    crit = mlrMBO::makeMBOInfillCritEI(),
    opt.focussearch.points = floor(search_space), # using 50% of possible search space
    opt.restarts = restarts
  )

  # We've got a lot of clunkiness to control the info from mlrMBO and friends
  # and keep a clean user experience
  # show.info = FALSE suppresses the info about the set up of the Latin Square
  if (debug >= 2) res <- mlrMBO::mbo(objF, des, surrogate, ctrl, show.info = TRUE)
  if (debug < 2) {
    mlr::configureMlr(show.learner.output = FALSE)
    suppressWarnings(res <- mlrMBO::mbo(objF, des, surrogate, ctrl, show.info = FALSE))
  }

  if (plot) plot(res)

  bestY <- res$x$x[1] # Note that the returned value of x1 corresponds to rows and hence y values
  bestX <- res$x$x[2]

  if (debug >= 1) cat("[ChemoSpec2D] Best alignment is to shift F2 by ", -bestX, " and F1 by ", -bestY, "\n\n")

  # Step 3. Create the modified mask
  # The mask (Mask) is "moved", not the reference
  # It's possible that we did not exceed the threshold in Step 1, but the best answer found by MBO is still
  # to not shift.  We have to capture that here as .shiftArray does not care for 0,0 input.

  if ((bestX == 0L) & (bestY == 0L)) {
    return(list(AA = Mask, shift = c(0L, 0L)))
  }

  # Note negation in next step
  dimnames(Mask) <- NULL
  Ash <- .shiftArray(Mask, xshift = -bestX, yshift = -bestY, fill = fill, NS = NS)
  dimnames(Ash) <- NULL

  return(list(AA = Ash, shift = c(-bestX, -bestY)))
}
