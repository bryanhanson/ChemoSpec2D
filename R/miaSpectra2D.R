#'
#' Multivariate Image Analysis (Tucker 1) of a Spectra2D Object
#'
#' Carry out multivariate image analysis of a \code{\link{Spectra2D}} object
#' (multivariate image analysis is the same as a Tucker1 analysis).
#' Function \code{\link[ThreeWay]{pcasup1}} from package \pkg{ThreeWay} is used.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @return A list per \code{\link[ThreeWay]{pcasup1}}.  Of particular interest are the
#'   elements \code{C} containing the eigenvectors and \code{1c} containing the eigenvalues.
#'   We add the class \code{mia} to the list for our use later, as well as a \code{method}
#'   element for annotating plots.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @seealso For other data reduction methods for \code{Spectra2D} objects, see
#' \code{\link{pfacSpectra2D}} and \code{\link{popSpectra2D}}.
#'
#' @references
#'
#' A. Smilde, R. Bro and P. Geladi
#' "Multi-way Analysis: Applications in the Chemical Sciences" Wiley (2004).
#' See especially Example 4.5.
#'
#' P. Geladi and H. Grahn "Multivariate Image Analysis" Wiley (1996).  Note that
#' in this text the meanings of scores and loadings are reversed from the usual
#' spectroscopic uses of the terms.
#'
#' @export
#' @importFrom ggplot2 ggtitle
#'
#' @examples
#' library("ggplot2")
#' data(MUD1)
#' res <- miaSpectra2D(MUD1)
#'
#' # plotScores & plotScree use ggplot2 graphics
#'
#' p1 <- plotScores(MUD1, res, tol = 1.0, ellipse = "cls")
#' p1 <- p1 + ggtitle("MIA Scores")
#' p1
#'
#' p2 <- plotScree(res)
#' p2
#'
#' # plotLoadings2D uses base graphics
#"
#' MUD1a <- plotLoadings2D(MUD1, res,
#'   load_lvls = seq(-90, 0, 10),
#'   main = "MIA Comp. 1 Loadings"
#' )
#'
#' # Selection of loading matrix levels can be aided by the following
#' # Use MUD1a$names to find the index of the loadings
#'
#' inspectLvls(MUD1a,
#'   which = 11, ylim = c(0, 80),
#'   main = "Histogram of Loadings Matrix"
#' )
miaSpectra2D <- function(spectra) {
  .chkArgs(mode = 21L)
  chkSpectra(spectra)

  if (!requireNamespace("ThreeWay", quietly = TRUE)) {
    stop("You must install package ThreeWay to use this function")
  }

  # Matricize: Frontal slices are concatenated left-to-right horizontally
  # Variables named as per ThreeWay::pcasup1
  n <- length(spectra$F1) # A-mode entries (I/rows)
  m <- length(spectra$F2) # B-mode entries (J/columns)
  p <- length(spectra$names) # C-mode entries (K/tubes)
  X <- matrix(NA_real_, nrow = n, ncol = m * p)

  for (i in 1:p) {
    st <- 1 + (i - 1) * m
    end <- i * m
    X[, st:end] <- spectra$data[[i]]
  }

  t1 <- ThreeWay::pcasup1(X, n, m, p, 3)
  t1$method <- "MIA"
  class(t1) <- "mia"

  return(t1)
}
