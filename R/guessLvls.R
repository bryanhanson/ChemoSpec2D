#'
#' Automatically Select Contour Levels for 2D NMR
#' 
#' Given a matrix for which one wants to make a contour plot, this
#' function will propose levels that are likely to work well
#' for 2D NMR data.  Uses \code{\link{calcLvls}} but makes
#' adjustments given the nature of 2D NMR data.
#'
#' @param M A matrix.
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
#' MM <- matrix(runif(100, -1, 1), nrow = 10) # test data
#' print(guessLvls(MM))
#' print(guessLvls(abs(MM)))
#' print(guessLvls(-1*abs(MM)))
#'
guessLvls <- function(M) {
	
  	# Choose the levels based upon the range of M
  	mode <- "log"
  	if (all(M < 0.0)) mode <- "neglog"
  	if (all(M > 0.0)) mode <- "poslog"
  	
  	lvls <- calcLvls(M, n = 10, mode = mode, lambda = 0.2)
  	
  	# Remove values near 0, these are too low for COSY-like data
  	if (mode == "poslog") lvls <- lvls[-1] # leaves 4 levels
  	if (mode == "neglog") lvls <- lvls[-length(lvls)] # leaves 4 levels
  	if (mode == "log") lvls <- lvls[-c(5, 6)] # leaves 8 levels

  invisible(lvls)
}
