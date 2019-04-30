#'
#' Align the Spectra in a Spectra2D Object using the HATS algorithm.
#' 
#' Align the spectra in a \code{\link{Spectra2D}} object using an implementation
#' of the HATS algorithm described by Robinette \emph{et al.}.  The algorithm
#' used here assumes the shift needed to align the spectra is relatively small and
#' performs well in such cases. If some spectra are significantly shifted, the approach
#' here does not perform that well.  More data sets are needed for testing to improve the
#' algorithm details, please share!
#'
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param maxF2 Integer.  The most extreme positive \code{F2step} to allow during the
#'        alignment process (units are data points).  The search stops if this value is exceeded.
#'
#' @param maxF1 Integer.  As for \code{maxF2}, but for F2.
#'
#' @param debug Integer.  Values >= 1 give messages about alignment progress.  Values >= 2
#'        print the merge matrix from the \code{hclust} object, which serves as the guide tree.
#'        For values >= 2 the same information is presented on the diagnostic plots.
#'
#' @param thres Numeric. The stopping criterion. Once the objective function reaches
#'        this value the search stops.  The objective function is the cosine of the angle
#'        between the unstacked spectra, so \code{thres} should be on [0 \ldots 1].
#'
#' @param iter Integer. The maximum number of iterations.
#'
#' @param method Character. Currently only \code{method = "CA"} is available which uses
#'               a coordinate ascent method to align the spectra.
#'
#' @param stopWhen Length two numeric vector.  The first value is a change in the
#'        objective function. Let's call it \code{v}.  The second value is a number of data points
#'        (let's call it \code{n}, coerced to integer). The search process is stopped when the average
#'        *change* of the most recent \code{n} values of the objective function falls below \code{v}.
#'
#' @param plot Logical. Shall a plot of the alignment progress be made?  The plot is useful for
#'        diagnostic purposes.  Every step of the alignment has a corresponding plot so you should
#'        probably direct the output to a pdf file.
#'
#' @return An object of S3 class \code{\link{Spectra2D}}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords multivariate
#'
#' @references
#' Roughly follows the algorithm described in Robinette et al. 2011
#'            \emph{Anal. Chem.} vol. 83, 1649-1657 (2011) dx.doi.org/10.1021/ac102724x
#'
#' @export
#' @importFrom stats hclust
#'

alignSpectra2D <- function(spectra, maxF2 = NULL, maxF1 = NULL,
  thres = 0.98, iter = 10, stopWhen = c(0.02, 5),
  plot = FALSE, debug = 0, method = "CA") {

  .chkArgs(mode = 21L)
  chkSpectra(spectra)
  if (is.null(maxF2)) maxF2 <- 20
  if (is.null(maxF1)) maxF1 <- 20

  # Step 1.  Unstack to 2D matrix
  M <- .unstack(spectra)
  
  # Step 2. Get the correlation matrix and compute dendrogram
  
  # hc <- hclust(as.dist(cor(t(M)))) # correlation is invariant to shift so it's probably not the best choice here
  hc <- hclust(rowDist(M, "cosine")) # cosine sensitive to shift, also on [-1,1]
  if (debug >= 2L) print(hc$merge)
  # if (plot == TRUE) plot(hc)
  
  # Step 3.  Create a list giving the spectra to be aligned at each step,
  #          working from the lowest point on the dendrogram
  
  AO <- .getAlignOrder(hc)
  
  # Step 4.  Align and replace the Mask with the aligned mask
    
  for (i in 1:length(AO)) {
  	if (debug >= 1L) message("Processing row ", i, " of ", length(AO), " from the guide tree:")
  	r <- AO[[i]][["Ref"]]
  	m <- AO[[i]][["Mask"]]
  	dtitle <- paste("Spectra", paste(m, collapse = ", "), "vs\n",  paste(r, collapse = ", "), sep = " ")
  	if (debug >= 1L) {
  		msg <- paste("Starting alignment of sample(s)", paste(m, collapse = ", "),
  		  "\n\twith sample(s)", paste(r, collapse = ", "), sep = " ")
  		cat(msg)
  	}
  	Ref <- .makeArray2(spectra, r)
  	Mask <- .makeArray2(spectra, m)
  	
  	if (method == "CA") {
  	  AA <- .globalAlignArraysCA(Ref, Mask,
  	  maxColShift = maxF2, maxRowShift = maxF1,
  	  debug = debug, title = dtitle)$AA
  	}

  	for (j in 1:dim(AA)[1]) { # get the number of slabs in AA
  	  spectra$data[[ m[j] ]] <- AA[j,,]
  	} 
  	if (debug >= 1L) cat("   ...Done!\n\n")
  }
  
  for (j in 1:length(spectra$names)) { # clean out attributes from .makeArray etc
    at <- names(attributes(spectra$data[[j]]))
  	bad_at <- which(at != "dim") # otherwise chkSpectra.Spectra2D complains
  	if (length(bad_at) > 0) attributes(spectra$data[[j]])[bad_at] <- NULL
  } 

  spectra <- .trimZeros(spectra) # SHOULD REPORT ON SOME LEVEL OF DEBUG
  
  chkSpectra(spectra)
  spectra
}

