#'
#' Scree Plots of MIA Results for a Spectra2D Object
#' 
#' Draw a scree plot to help choose the number of components in a MIA
#' (multivariate image analysis).
#'
#' @param results A list as produced by \code{\link{miaSpectra2D}}.  Of particular interest are the
#'   elements \code{C} containing the eigenvectors and \code{1c} containing the eigenvalues.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @references The idea for the alternative style scree plot produced here
#' came from the NIR-Quimiometria blog by jrcuesta, at
#' \url{https://nir-quimiometria.blogspot.com/2012/02/pca-for-nir-spectrapart-004-projections.html}
#' 
#' @keywords multivariate hplot
#'
#' @seealso Please see \code{\link{miaSpectra2D}} for more information and examples.
#' 
#' @export
#'
#' @importFrom graphics plot axis points abline legend text

plotScree <- function(results, ...) {

	if (missing(results)) stop("No MIA results provided")
	
	if (class(results) == "pcasup1")  {
		# compute cumulative variance
		lc <- results$lc
		cumvariance <- cumsum(lc)/sum(lc) * 100
		ncp <- length(cumvariance)
		if (ncp > 10) ncp <- 10
		
		# main plot
		plot(rep(1:ncp, each = nrow(results$C)), as.vector(results$C[,1:ncp]), type = "p",
			col = "red", xlab = "component", ylab = "scores",
			xlim = c(1, ncp+0.5), cex = 0.5, xaxt = "n", ...)
		axis(1, at = c(1:ncp), labels = TRUE)
		
		# label with cumulative variance
		lab.txt <- paste(round(cumvariance[1:ncp], 0), "%", sep = "")
		y.pos <- apply(results$C[,1:ncp], MARGIN = 2, FUN = range)
		y.pos <- y.pos[2,]
		y.max <- max(y.pos)
		off <- 0.1 * y.max
		text(c(1:ncp) + 0.35, off, labels = lab.txt, cex = 0.75)
		abline(h = 0, lty = "dashed", col = "gray")
	
		# legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)
		legend("topright", y = NULL, "cumulative percent variance shown to right of PC", bty = "n", cex = 0.75)

	} # end of class(results) == "pcasup1"
	
	
}

