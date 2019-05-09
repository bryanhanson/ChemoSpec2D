#'
#' Globally Align 2D NMR Spectra, Including Arrays Composed of Stacks of 2D Spectra
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
#'
#' @param plot Logical. Shall a plot of the alignment progress be made?  The plot is useful for
#'        diagnostic purposes.
#'
#' @return A list, with two elements: \code{AA} the aligned matrix, which is a shifted version
#'         of the mask (Mask), and \code{diagnostics}, a data frame providing information
#'         about the history of the alignment (and is the basis for the diagnostic plot).
#'
#' @importFrom smoof makeSingleObjectiveFunction
#' @importFrom lhs generateDesign
#' @importFrom mlr makeLearner
#' @importFrom mlrMBO mbo makeMBOcontrol setMBOControlTermination setMBOControlInfill
#'
#' @export
#' @noRd
#'

.AlignArraysMBO <- function(Ref, Mask,
	maxColShift = 40, maxRowShift = 40,
	thres = 0.99, iter = 10L, plot = FALSE) {

  if (!all(dim(Ref)[2:3] == dim(Mask)[2:3])) stop("Arrays Ref and Mask must have the same dimensions")
     
  # Step 1. Check to see, if by chance, the matrices are perfectly aligned with no shifts.
  # If so, we can skip the optimization process, and exit now

  currOF <- .evalArrayOverlap2(c(0L, 0L))
  if (currOF >= thres) return(AA = Mask)
  
  # Step 2.  Run MBO to find best overlap
  
  objF <- makeSingleObjectiveFunction(
    name = "Eval Array Overlap",
    fn = .evalArrayOverlap2,
    minimize = FALSE,
    par.set = makeParamSet(
      makeIntegerVectorParam(
        len = 2L,
        id = "x",
        lower = c(-maxColShift, -maxRowShift),
        upper = c(maxColShift, maxRowShift)
        )
    )
  )
  
  des <- generateDesign(n = 10L, getParamSet(objF), fun = randomLHS)
  surrogate = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = iter)
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

  res <- mbo(objF, des, surrogate, ctrl)

  # Step 3. Create the modified mask
  # The mask (Mask) is "moved", not the reference
    
  Ash <- .shiftArray(Mask, res$x[1], res$x[2], fill = "zero")
  dimnames(Ash) <- NULL
  return(AA = Ash)
}

