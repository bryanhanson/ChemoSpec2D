#'
#' Automatically Select Contour Levels for 2D NMR
#' 
#' Given a matrix for which one wants to make a contour plot, this
#' function will propose levels that are likely to work well
#' for 2D NMR data.  Uses \code{\link{calcLvls}} but makes
#' adjustments given the nature of 2D NMR data (i.e. this function
#' uses \code{mode = "log"} and removes the lowest contour level).
#'
#' @param M A matrix.
#'
#' @param \dots Additional parameters to be passed to \code{\link{calcLvls}}.
#'
#' @return A vector of the suggested contour levels (invisibly).
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utility
#'
#' @seealso Please see \code{\link{calcLvls}}.
#' 
#' @export
#'
#' @examples
#' 
#' set.seed(9)
#' MM <- matrix(rnorm(100, sd = 2), nrow = 10) # test data
#' guessLvls(MM, showHist = TRUE) # showHist passes through to calcLvls
#' guessLvls(abs(MM), showHist = TRUE)
#'
guessLvls <- function(M, ...) {
	
  	# Choose the levels based upon the range of M
  	mode <- "log"
  	if (all(M < 0.0)) mode <- "neglog"
  	if (all(M > 0.0)) mode <- "poslog"
  	
  	lvls <- calcLvls(M, n = 10, mode = mode, lambda = 0.2, ...)
  	
  	# Remove values near 0, these are typically too low
  	if (mode == "poslog") lvls <- lvls[-1] # leaves 4 levels
  	if (mode == "neglog") lvls <- lvls[-length(lvls)] # leaves 4 levels
  	if (mode == "log") lvls <- lvls[-c(5, 6)] # leaves 8 levels

  invisible(lvls)
}
