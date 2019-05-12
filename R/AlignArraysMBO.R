#'
#' Globally Align Arrays, Including Arrays Composed of Stacks of 2D NMR Spectra
#'
#' This function attempts to align (multiple) 2D NMR spectra, storing the spectra as arrays.
#' Alignment occurs on a discrete grid corresponding to the indices. The arrays are arranged 
#' such that one is aligning their projection onto a 2D grid, which means one is shifting the
#' arrays in 2D -- essentially the problem reduces to aligning matrices along their indices.
#' The potential alignment space runs from \code{-maxColShift} \ldots \code{maxColShift} and \code{-maxRowShift}
#' \ldots \code{maxRowShift}.
#'
#' The strategy used is "Model Based Optimization" or MBO, which is also known as "Bayesian Optimization"
#' or BO.  The search for the optimum alignment starts by checking if no shift exceeds the threshold.  If so,
#' no search is needed.  If the threshold is not exceeded, we proceed to MBO/BO.
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
#' @param plot Logical. Shall a plot of the alignment progress be made?  The plot is useful for
#'        diagnostic purposes.  Every step of the alignment has a corresponding plot so you should
#'        probably direct the output to a pdf file.
#'
#' @return A list with two elements: 1. The aligned matrix, which is a shifted version of the mask (AA).
#'         2. A length two integer vector containing the optimal x and y shifts (shift).
#'
# #' @importFrom smoof makeSingleObjectiveFunction
# #' @importFrom ParamHelpers generateDesign makeParamSet makeIntegerVectorParam getParamSet
# #' @importFrom mlr makeLearner
# #' @importFrom lhs randomLHS
# #' @importFrom mlrMBO mbo makeMBOControl setMBOControlTermination setMBOControlInfill makeMBOInfillCritEI
#'
#' @export
#' @noRd
#'

.AlignArraysMBO <- function(Ref, Mask,
	maxColShift = 40, maxRowShift = 40,
	thres = 0.99, no.it = 20L, restarts = 2L,
	plot = FALSE, debug = 0L) {

  if (!all(dim(Ref)[2:3] == dim(Mask)[2:3])) stop("Arrays Ref and Mask must have the same dimensions")
     
  # Step 1. Check to see, if by chance, the matrices are perfectly aligned with no shifts.
  # If so, we can skip the optimization process, and exit now.

  currOF <- .evalArrayOverlapMBO(c(0L, 0L))
  if (currOF >= thres) return(list(AA = Mask, shift = c(0L, 0L)))
  
  # Step 2.  Run MBO to find best overlap

  makeF <- function(Ref, Mask) {
  	force(Ref)
  	force(Mask)
  	# see .evalArrayOverlapMBO for the mechanism of retrieving Ref and Mask from the calling environment
    function(x) {.evalArrayOverlapMBO(x)}
  }
  
  optFun <- makeF(Ref, Mask)
  
  objF <- smoof::makeSingleObjectiveFunction(
    name = "Eval Array Overlap MBO",
    fn = optFun,
    minimize = FALSE,
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerVectorParam(
        len = 2L,
        id = "x",
        lower = c(-maxColShift, -maxRowShift),
        upper = c(maxColShift, maxRowShift)
        )
    )
  )
  
  search_space = (2*maxColShift + 1) * (2*maxRowShift + 1)
    
  des <- ParamHelpers::generateDesign(n = 20L, ParamHelpers::getParamSet(objF), fun = lhs::randomLHS)
  surrogate <- mlr::makeLearner("regr.km",
    predict.type = "se",
    covtype = "matern3_2",
    config = list(show.learner.output = FALSE))
  ctrl <- mlrMBO::makeMBOControl()
  ctrl <- mlrMBO::setMBOControlTermination(ctrl, iters = no.it)
  ctrl <- mlrMBO::setMBOControlInfill(ctrl, crit = mlrMBO::makeMBOInfillCritEI(),
    opt.focussearch.points = floor(search_space),  # using 50% of points
    opt.restarts = restarts)

  # next line: show.info = FALSE suppresses the info about the set up of the Latin Square
  res <- mlrMBO::mbo(objF, des, surrogate, ctrl, show.info = TRUE)
  if (plot) plot(res)
  
  bestY <- res$x$x[1] # Note that the returned value of x1 corresponds to rows and hence y values
  bestX <- res$x$x[2]
  
  if (debug >= 1) cat("[ChemoSpec2D] Best alignment is to shift F2 by ", -bestX, " and F1 by ", -bestY, "\n\n")
  
  # Step 3. Create the modified mask
  # The mask (Mask) is "moved", not the reference
  # It's possible that we did not exceed the threshold in Step 1, but the best answer is still
  # to not shift.  We have to capture that here as .shiftArray does not care for such input.
  
  if ((bestX == 0L) & (bestY == 0L)) return(list(AA = Mask, shift = c(0L, 0L)))
  
  # Note negation in next step
  Ash <- .shiftArray(Mask, xshift = -bestX, yshift = -bestY, fill = "zero")
  dimnames(Ash) <- NULL
  
  return(list(AA = Ash, shift = c(-bestX, -bestY)))
}

