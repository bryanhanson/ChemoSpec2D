#'
#' PARAFAC Analysis of a Spectra2D Object
#' 
#' Carry out PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' Function \code{\link[multiway]{parafac}} from \pkg{multiway} is used.
#' For large data sets, computational time may be long enough that
#' it mmight desirable to run in batch mode.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param parallel Logical.  Should parallel processing be used?
#'        Unless you love waiting, you should use parallel processing for real data sets.
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
#' Laboratory Systems} vol. 38 pgs. 149-171 (1997).
#'
#'  A. Smilde, R. Bro and P. Geladi
#' "Multi-way Analysis: Applications in the Chemical Sciences" Wiley (2004).
#'
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
#' pfacScores(MUD1, res, main = "PARAFAC Score Plot")
#' pfacLoadings(MUD1, res, main = "PARAFAC Comp. 1 Loadings\nDefault Levels")
#' pfacLoadings(MUD1, res, ref = 6L,
#'   main = "PARAFAC Comp. 1 Loadings + Ref. Spectrum\nDefault Levels")
#' pfacLoadings(MUD1, res, load_lvls = c(0.7, 0.8, 0.9),
#'   ref = 6L, ref_lvls = 0.5,
#'   main = "PARAFAC Comp. 1 Loadings + Ref. Spectrum\nCustom Levels; Ref. Spectrum Black")
#'

pfacSpectra2D <- function(spectra, parallel = TRUE, ...) {

  if (class(spectra) != "Spectra2D") stop("spectra argument was not a Spectra2D object")
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

