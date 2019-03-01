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
#'        Unless you love waiting, you should use parallel processing for larger data sets.
#'
#' @param setup Logical.  If \code{TRUE} the parallel environment will be automatically
#'        configured for you.  If \code{FALSE}, the user must configure the environment
#'        themselves (desirable for instance if working on Azure or AWS EC2).
#'
#' @param \dots Additional parameters to be passed to function \code{\link[multiway]{parafac}}.
#'        At the minimum, you'll need to specify \code{nfac}.  You should also give thought to
#'        value of \code{const}, allowed options can be seen in \code{\link[CMLS]{const}}.
#'        The default is to compute an unconstrained solution.  However, in some cases one may
#'        wish to apply a non-negativity constraint.
#'
#' @return An object of class \code{parafac}.
#'
#' @section Warning:
#'   To get reproducible results you will need to \code{set.seed()}.  See the example.
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
#' @importFrom parallel makeCluster clusterEvalQ stopCluster detectCores clusterSetRNGStream
#'
#' @examples
#'
#' data(MUD1)
#' set.seed(123)
#' res <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#' plotScores(MUD1, res, tol = 0.1, leg.loc = "topright",
#'   ellipse = "cls", main = "PARAFAC Score Plot")
#' res1 <- pfacLoadings(MUD1, res,
#'   load_lvls = seq(-12, -2, 2),
#'   main = "PARAFAC Comp. 1 Loadings")
#' res2 <- pfacLoadings(MUD1, res, load_lvls = seq(-12, -2, 2),
#'   ref = 2, ref_lvls = c(-0.2, -0.1, 0.1, 0.2),
#'   ref_cols = c("violet", "violet", "orange", "orange"),
#'   main = "PARAFAC Comp. 1 Loadings + Ref. Spectrum")
#'
#' # Selection of loading matrix levels can be aided by the following
#'
#' inspectLvls(res1, loadings = TRUE, ylim = c(0, 50),
#'   main = "Histogram of Loadings Matrix")
#'

pfacSpectra2D <- function(spectra, parallel = FALSE, setup = FALSE, ...) {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  if ((!parallel) & (setup)) stop("setup should not be TRUE if parallel is FALSE")
  
  if (!requireNamespace("multiway", quietly = TRUE)) {
    stop("You must install package multiway to use this functoin")
  }

  # Set up data array (frontal slices)
  DA <- .makeArray(spectra)
  if (any(is.na(DA))) stop("Data for parafac cannot have NA")

  # Set up parallel environment
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("You must install package parallel to use the parallel option")
    }
    if (setup) {
      cl <- makeCluster(detectCores())
      ce <- clusterEvalQ(cl, library(multiway))
      clusterSetRNGStream(cl, 123)
    }
  }
  
  # Run it
  if (!parallel) pfac <- parafac(DA, ...)
  if (parallel) {
  	if (setup) pfac <- parafac(DA, parallel = TRUE, cl = cl, ...)
  	if (!setup) pfac <- parafac(DA, parallel = TRUE, ...) # user provides add'l args
  	
  }
  # Wrap up
  if (setup) stopCluster(cl)
  return(pfac)
}

