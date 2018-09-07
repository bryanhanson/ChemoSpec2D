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
#'   We add the class \code{pcasup1} to the list for our use later.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @references
#'
#' A. Smilde, R. Bro and P. Geladi
#' "Multi-way Analysis: Applications in the Chemical Sciences" Wiley (2004).
#' See especially Example 4.5.
#'
#' P. Geladi and H. Grahn "Multivariate Image Analysis" Wiley (1996).
#'
#'
#' @export
#'
#' @importFrom ThreeWay pcasup1
#'
#' @examples
#'
#' data(MUD1)
#' res <- miaSpectra2D(MUD1)
#' miaScores(MUD1, res, main = "MIA Scores")
#' plotScree(res)
#'

miaSpectra2D <- function(spectra) {

  if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
  chkSpectra2D(spectra)
  
  if (!requireNamespace("ThreeWay", quietly = TRUE)) {
    stop("You must install package ThreeWay to use this function")
  }
  
  # Matricize: Frontal slices are concatenated left-to-right horizontally
  # Variables named as per ThreeWay::pcasup1
  n <- length(spectra$F1) # A-mode entries (I/rows)
  m <- length(spectra$F2) # B-mode entries (J/columns)
  p <- length(spectra$names) # C-mode entries (K/tubes)
  X <- matrix(NA_real_, nrow = n, ncol = m*p)

  for (i in 1:p) {
   st <- 1 + (i - 1) * m
   end <- i * m
   X[,st:end] <- spectra$data[[i]]
  }

  t1 <- ThreeWay::pcasup1(X, n, m, p, 3)
  class(t1) <- "pcasup1"
  
  return(t1)
}

