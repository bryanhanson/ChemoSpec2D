#'
#' PARAFAC Analysis of a Spectra2D Object
#' 
#' Carry out PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' Function \code{\link[multiway]{parafac}} from \pkg{multiway} is used.
#' Because computation is slow, it is advisable to run this in batch
#' mode from the command line.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param parallel Logical.  Should parallel processing be used?
#'        Unless you love waiting, you should use parallel processing.
#'
#' @param \dots Additional parameters to be passed to function \code{\link[multiway]{parafac}}.
#'        At the minimum, you'll need to specify \code{nfac}.
#'
#' @return An object of class \code{parafac}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @references R. Bro "PARAFAC. Tutorial and applications" \emph{Chemometrics and Intelligent
#' Laboratory Systems} vol. 38 pgs. 149-171 (1997).  A. Smilde, R. Bro and P. Geladi
#' "Multi-way Analysis: Applications in the Chemical Sciences" Wiley (2004).
#'
#' @export
#'
#' @importFrom multiway parafac
#' @importFrom parallel makeCluster clusterEvalQ stopCluster detectCores
#'
#' @examples
#'
#' data(MUD1)
#' res <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#' pfacScores(MUD1, res)
#' pfacLoadings(MUD1, res)
#'

pfacSpectra2D <- function(spectra, parallel = TRUE, ...) {

  chkSpectra2D(spectra)
  
  if (!requireNamespace("multiway", quietly = TRUE)) {
    stop("You must install package multiway to use this functoin")
  }

  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("You must install package parallel to use the parallel option")
    }
    cl <- makeCluster(detectCores())
    ce <- clusterEvalQ(cl, library(multiway))
  }
  
  # Set up data array (frontal slices)
  DA <- array(unlist(spectra$data), dim = c(length(spectra$F2), length(spectra$F1), length(spectra$names)))
  dimnames(DA) <- list(A = NULL, B = NULL, C = spectra$names)
  if (any(is.na(DA))) stop("Data for parafac cannot have NA")
  
  # Run it
  if (!parallel) pfac <- parafac(DA, const = c(2, 2, 2), ...)
  if (parallel) pfac <- parafac(DA, const = c(2, 2, 2), parallel = TRUE, cl = cl, ...)
  
  # Wrap up
  if (parallel) stopCluster(cl)
  return(pfac)
}

