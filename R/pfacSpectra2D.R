#'
#' PARAFAC Analysis of a Spectra2D Object
#' 
#' Carry out PARAFAC analysis of a \code{\link{Spectra2D}} object.
#' Function \code{\link[multiway]{parafac}} from \pkg{multiway} is used.
#' For large data sets, computational time may be long enough that
#' it might desirable to run in batch mode and possibly use parallel processing.
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param parallel Logical.  Should parallel processing be used?
#'        Unless you love waiting, you should use parallel processing for larger data sets.
#'        If you are working on a shared machine and/or another process (possibly spawned by
#'        your request downstream) might also try to access all or some of the cores in your CPU,
#'        you should be careful to avoid hogging the cores.
#'        \code{parallel::detectCores()} will tell you how many cores are available to everyone.
#'        You can run \code{options(mc.cores = 2)} to set the number of cores this function will use.
#'
#' @param setup Logical.  If \code{TRUE} the parallel environment will be automatically
#'        configured for you.  If \code{FALSE}, the user must configure the environment
#'        themselves (desirable for instance if working on Azure or AWS EC2).
#'
#' @param nfac Integer.  The number of factors/components to compute.
#'
#' @param \dots Additional parameters to be passed to function \code{\link[multiway]{parafac}}.
#'        You should give thought to value of \code{const}, allowed options can be seen in
#'        \code{\link[CMLS]{const}}. The default is to compute an unconstrained solution.
#'        However, in some cases one may wish to apply a non-negativity constraint.  Also,
#'        to suppress the progress bar, you can use \code{verbose = FALSE}.
#'
#' @return An object of class \code{pfac} and \code{parafac}, modified to include a list
#' element called \code{$method} which is \code{parafac}.
#'
#' @section Warning:
#'   To get reproducible results you will need to \code{set.seed()}.  See the example.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso For other data reduction methods for \code{Spectra2D} objects, see
#' \code{\link{miaSpectra2D}} and \code{\link{popSpectra2D}}.
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
#' @importFrom parallel makeCluster clusterEvalQ stopCluster detectCores clusterSetRNGStream
#'
#' @examples
#'
#' data(MUD1)
#' set.seed(123)
#' res <- pfacSpectra2D(MUD1, parallel = FALSE, nfac = 2)
#' plotScores(MUD1, res, leg.loc = "topright",
#'   ellipse = "cls", main = "PARAFAC Score Plot")
#' res1 <- plotLoadings2D(MUD1, res,
#'   load_lvls = c(1, 5, 10, 15, 25),
#'   main = "PARAFAC Comp. 1 Loadings")
#' res2 <- plotLoadings2D(MUD1, res, load_lvls = c(1, 5, 10, 15, 25),
#'   ref = 2, ref_lvls = seq(5, 35, 5),
#'   ref_cols = rep("black", 7),
#'   main = "PARAFAC Comp. 1 Loadings + Ref. Spectrum")
#'
#' # Selection of loading matrix levels can be aided by the following
#' # Use res1$names to find the index of the loadings
#'
#' inspectLvls(res1, which = 11, ylim = c(0, 50),
#'   main = "Histogram of Loadings Matrix")
#'

pfacSpectra2D <- function(spectra, parallel = FALSE, setup = FALSE, nfac = 2, ...) {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  if ((!parallel) & (setup)) stop("setup should not be TRUE if parallel is FALSE")
  
  if (!requireNamespace("multiway", quietly = TRUE)) {
    stop("You must install package multiway to use this function")
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
      # getOption strategy per H. Bengtsson to avoid hogging all cores (on twitter)
      cl <- makeCluster(getOption("mc.cores", max(detectCores(), na.rm = TRUE)))
      ce <- clusterEvalQ(cl, library(multiway))
      clusterSetRNGStream(cl, 123)
    }
  }
  
  # Run it
  if (!parallel) pfac <- multiway::parafac(DA, nfac = nfac, ...)
  if (parallel) {
  	if (setup) pfac <- multiway::parafac(DA, parallel = TRUE, cl = cl, nfac = nfac, ...)
  	if (!setup) pfac <- multiway::parafac(DA, parallel = TRUE, nfac = nfac, ...) # user provides add'l args
  	
  }
  # Wrap up
  if (setup) stopCluster(cl)
  pfac$method <- "parafac"
  class(pfac) <- c(class(pfac), "pfac")
  return(pfac)
}

