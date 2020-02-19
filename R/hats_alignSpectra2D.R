#'
#' Align the Spectra in a Spectra2D Object using the HATS algorithm.
#'
#' Align the spectra in a \code{\link{Spectra2D}} object using an implementation
#' of the HATS algorithm described by Robinette \emph{et al.}.  Currently, only
#' global, not local, alignment is carried out.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param maxF2 Integer.  The most extreme positive \code{F2step} to allow during the
#'        alignment process (units are data points).  Search for the optimal alignment will
#'        cover the region \code{-maxF2} \ldots \code{maxF2} and \code{-maxF1}
#'        \ldots \code{maxF1}.  Defaults to about 10\% of the number of points.
#'
#' @param maxF1 Integer.  As for \code{maxF2}, but for F1.
#'
#' @param debug Integer.
#' \itemize{
#'   \item Values >= 1 give messages about alignment progress in black text.
#'   \item Values >= 2 print the merge matrix from the \code{hclust} object, if \code{plot}
#'         is also \code{TRUE}.  This is the guide tree.
#'   \item For \code{method = "MBO"} values less than 2 suppress some messages and warnings from
#'        the underlying functions.  If the alignment doesn't work well, set \code{debug = 2}.
#'   \item Setting \code{plot = TRUE} also gives a view of alignment diagnostics.
#' }
#'
#' @param thres Numeric. Prior to launching the optimization, the objective function is evaluated
#'        for no shift in case this is actually the best alignment (saving a great deal of time).
#'        If this initial check exceeds the value of \code{thres} (when \code{minimize = FALSE}),
#'        or is below \code{thres} when \code{minimize = TRUE}, no optimization is performed
#'        and the unshifted spectra are returned.
#'
#' @param no.it Integer. The maximum number of iterations in the optimization.
#'
#' @param restarts Integer. The maximum number of independent rounds of optimization.
#'
#' @param method Character. Currently only \code{method = "MBO"} is available which uses
#'        the HATS algorithm plus model based optimization (aka Bayesian optimization) method to
#'        align the spectra. Use \code{plot = TRUE} to see this in action.
#'
#' @param dist_method Character. The distance method to use in the objective function.
#'        See \code{\link{rowDist}} for options.
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
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @section Advice:
#' \itemize{
#'   \item I suggest that you plot your possibly mis-aligned spectra first, zooming in on a small region where
#'         alignment might be an issue, and get an idea of the size of the alignment problem.  This will help
#'         you choose good values for \code{maxF1} and \code{maxF2} which will speed up the search.
#'   \item The algorithm uses random numbers to initialize the search, so set the seed for reproducible results.
#'         Different seeds may give different results; you may find it useful to experiment a bit and see how
#'         the alignment turns out.
#'   \item Be sure that your choice of \code{thres}, \code{minimize} and \code{dist_method} are self-consistent.
#'         Some \code{dist_method} choices are bounded, others unbounded, and some should be minimized, others maximized.
#'   \item You should use \code{\link{sampleDist}} to visualize the distances ahead of time.  The method
#'         chosen should return a wide numerical range between samples or it won't give a good alignment result.
#' }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @references
#' Roughly follows the algorithm described in Robinette et al. 2011
#'   \emph{Anal. Chem.} vol. 83, 1649-1657 (2011) dx.doi.org/10.1021/ac102724x
#'
#' @export
#' @importFrom stats hclust
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(MUD2)
#' sumSpectra(MUD2)
#' mylvls <- seq(3, 30, 3)
#'
#' # Plot before alignment
#' plotSpectra2D(MUD2,
#'   which = c(2, 3, 5, 6), showGrid = TRUE,
#'   lvls = LofL(mylvls, 4),
#'   cols = LofC(c("black", "red", "blue", "green"), 4, 10, 2)
#' )
#'
#' # Carry out alignment
#' # You might want to direct the diagnostic output here to a pdf file
#' # This alignment takes about 90 seconds including the plotting overhead
#' MUD2a <- hats_alignSpectra2D(MUD2, method = "MBO", debug = 1, plot = TRUE)
#'
#' # Plot after alignment
#' plotSpectra2D(MUD2a,
#'   which = c(2, 3, 5, 6), showGrid = TRUE,
#'   lvls = LofL(mylvls, 4),
#'   cols = LofC(c("black", "red", "blue", "green"), 4, 10, 2)
#' )
#' }
#'
hats_alignSpectra2D <- function(spectra, maxF2 = NULL, maxF1 = NULL,
                                dist_method = "cosine", minimize = FALSE, thres = 0.99,
                                no.it = 20L, restarts = 2L, method = "MBO", fill = "noise",
                                plot = FALSE, debug = 1) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  if (is.null(maxF2)) maxF2 <- floor(0.1 * length(spectra$F2))
  if (is.null(maxF1)) maxF1 <- floor(0.1 * length(spectra$F1))

  msg <- "This is a beta version of hats_alignSpectra2D.
    You should set the seed for reproducible results.
    Please check your results carefully, and consider sharing your data
    for additional testing.  Contact Bryan Hanson via hanson@depauw.edu"
  message(msg)

  if (!requireNamespace("mlrMBO", quietly = TRUE)) {
    stop("You must install package mlrMBO to use this function")
  }

  if (!requireNamespace("smoof", quietly = TRUE)) {
    stop("You must install package smoof to use this function")
  }

  if (!requireNamespace("ParamHelpers", quietly = TRUE)) {
    stop("You must install package ParamHelpers to use this function")
  }

  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("You must install package mlr to use this function")
  }

  if (!requireNamespace("lhs", quietly = TRUE)) {
    stop("You must install package lhs to use this function")
  }


  # Step 1.  Unstack to 2D matrix
  M <- .unstack(spectra)

  # Step 2. Compute dendrogram using specified distance

  hc <- hclust(rowDist(M, dist_method))
  if (debug >= 2L) print(hc$merge)
  if (plot == TRUE) plot(hc)

  # Step 3a.  Create a list giving the spectra to be aligned at each step,
  #          working from the lowest point on the dendrogram

  AO <- .getAlignOrder(hc)

  # Step 3b. If fill = "noise" compute the noise surface (takes a bit of time, do it once)

  if (fill == "noise") NS <- .noiseSurface(spectra)

  # Step 4.  Align and replace the Mask with the aligned mask

  if (debug >= 1L) {
    DiagDF <- data.frame(
      Ref = rep(NA_character_, length(AO)),
      Mask = rep(NA_character_, length(AO)),
      F2shift = rep(NA_integer_, length(AO)),
      F1shift = rep(NA_integer_, length(AO)),
      stringsAsFactors = FALSE
    )
  }

  for (i in 1:length(AO)) {
    if (debug >= 1L) cat("[ChemoSpec2D] Processing row ", i, " of ", length(AO), " from the guide tree:\n")
    r <- AO[[i]][["Ref"]]
    m <- AO[[i]][["Mask"]]

    if (debug >= 1L) {
      msg <- paste("[ChemoSpec2D] Starting alignment of sample(s)", paste(m, collapse = ", "),
        "\n\twith sample(s)", paste(r, collapse = ", "), "\n",
        sep = " "
      )
      cat(msg)
      DiagDF[i, "Ref"] <- paste(r, collapse = ", ")
      DiagDF[i, "Mask"] <- paste(m, collapse = ", ")
    }

    Ref <- .makeArray2(spectra, r)
    Mask <- .makeArray2(spectra, m)

    # If fill != noise the zeros will be added later in .shiftArray
    if (fill == "noise") NS2 <- NS[m, , , drop = FALSE] else NS2 <- NULL

    if (method == "MBO") {
      mlr::configureMlr() # see github.com/mlr-org/mlr/issues/2141
      MBO <- .AlignArraysMBO(Ref, Mask,
        maxColShift = maxF2, maxRowShift = maxF1,
        dist_method = dist_method, minimize = minimize, thres = thres,
        no.it = no.it, restarts = restarts,
        fill = fill, NS = NS2,
        plot = plot, debug = debug
      )
      if (debug >= 1L) {
        DiagDF[i, "F2shift"] <- MBO$shift[1]
        DiagDF[i, "F1shift"] <- MBO$shift[2]
      }
    }

    AA <- MBO$AA

    for (j in 1:dim(AA)[1]) { # get the number of slabs in AA
      spectra$data[[m[j]]] <- AA[j, , ]
    }
  } # end of traversing guide tree

  for (j in 1:length(spectra$names)) { # clean out attributes from .makeArray etc
    at <- names(attributes(spectra$data[[j]]))
    bad_at <- which(at != "dim") # otherwise chkSpectra.Spectra2D complains
    if (length(bad_at) > 0) attributes(spectra$data[[j]])[bad_at] <- NULL
  }

  if (debug >= 1L) {
    cat("[ChemoSpec2D] Alignment steps and results:\n")
    print(DiagDF)
  }

  chkSpectra(spectra)
  spectra
}
