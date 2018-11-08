#'
#' PARAFAC Analysis of a Spectra2D Object
#' 
#' Carry out PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' Function \code{\link[multiway]{parafac}} from \pkg{multiway} is used.
#' For large data sets, computational time may be long enough that
#' it might desirable to run in batch mode.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param parallel Logical.  Should parallel processing be used?
#'        Unless you love waiting, you should use parallel processing for real data sets.
#'
#' @param \dots Additional parameters to be passed to function \code{\link[multiway]{parafac}}.
#'        At the minimum, you'll need to specify \code{nfac}.  You should also give thought to
#'        value of \code{const}, allowed options can be seen in \code{\link[CMLS]{const}}.
#'        The default is to compute an unconstrained solution.  However, in some cases one may
#'        wish to apply a non-negativity constraint.
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
#' set.seed(123)
#' res <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#' plotScores(MUD1, res, tol = 0.1, leg.loc = "bottomright", main = "PARAFAC Score Plot")
#' res1 <- pfacLoadings(MUD1, res, load_lvls = seq(5, 30, 5),
#'   main = "PARAFAC Comp. 1 Loadings")
#' res2 <- pfacLoadings(MUD1, res, load_lvls = seq(5, 30, 5),
#'   ref = 6, ref_lvls = c(-0.5, 0.5), ref_cols = c("blue", "red"),
#'   main = "PARAFAC Comp. 1 Loadings + Ref. Spectrum")
#'
#' # Selection of loading matrix levels can be aided by the following
#'
#' inspectLvls(res1, loadings = TRUE, ylim = c(0, 50),
#'   main = "Histogram of Loadings Matrix")
#'

pfacSpectra2D <- function(spectra, parallel = TRUE, ...) {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  
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
  DA <- .makeArray(spectra)
  if (any(is.na(DA))) stop("Data for parafac cannot have NA")
  
  # Run it
  if (!parallel) pfac <- parafac(DA, ...)
  if (parallel) pfac <- parafac(DA, parallel = TRUE, cl = cl, ...)
 
  # Wrap up
  if (parallel) stopCluster(cl)
  return(pfac)
}

