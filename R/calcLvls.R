#'
#' Calculate Levels for Contour and Image Type Plots
#' 
#' Given a matrix or vector input, this function will assist in selecting levels for preparing
#' contour and image type plots.  For instance, levels can be spaced evenly,
#' logrithmically, exponentially or using a cumulative distribution function.
#' \code{NA} values are ignored.
#' 
#' @param M A numeric matrix or vector.
#'
#' @param n For all methods except \code{ecdf}, an integer giving the number of
#' levels desired.  For most modes this is used interally as \code{floor(n/2)}
#' and the result doubled.  In addition,
#' only the positive or negative levels may be selected, so you are not likely
#' to actually get \code{n} levels most of the time (remember, you can always give
#' your desired levels as a vector).  For \code{ecdf}, \code{n} should be one or
#' more values in
#' the interval [0...1].  For instance, a value of 0.6 corresponds to a single
#' level in which 60 percent of the matrix values are below, and 40 percent
#' above.
#'
#' @param mode Character.  One of \code{"even"}, \code{"log"}, \code{"exp"},
#' \code{"ecdf"}, \code{"posexp"}, \code{"negexp"}, \code{"poslog"}, \code{"neglog"}
#' or \code{NMR}.
#' \code{"even"} will create evenly
#' spaced levels.  \code{"log"} will create levels which are more closely
#' spaced at the high values, while \code{"exp"} does the opposite.  The pos- or
#' neg- versions select just the positive or negative values.  \code{"ecdf"}
#' computes levels at the requested quantiles of the matrix. \code{NMR} uses
#' \code{log}, \code{base = 1} and \code{n = 48}.
#'
#' @param lambda Numeric.  A non-zero exponent used with \code{method = "exp"}
#' and relatives.
#'
#' @param base Integer.  The base used with \code{method = "log"} and
#' relatives.
#'
#' @param showHist Logical.  Shall a histogram be drawn showing the location of
#' the chosen levels?
#'
#' @param \dots Arguments to be passed downstream.
#'
#' @return A numeric vector giving the levels.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @importFrom graphics hist par layout plot
#' @importFrom grDevices rainbow
#' @importFrom stats ecdf
#'
#' @export
#'
#' @keywords utilities
#'
#' @examples
#' 
#' set.seed(9)
#' MM <- matrix(runif(100, -1, 1), nrow = 10) # test data
#' tsts <- c("even", "log", "poslog", "exp", "posexp", "ecdf", "NMR")
#' for (i in 1:length(tsts)) {
#' 	nl <- 10
#' 	if(tsts[i] == "ecdf")  nl <- seq(0.1, 0.9, 0.1)
#' 	levels <- calcLvls(M = MM, n = nl, mode = tsts[i],
#'    showHist = TRUE, main = tsts[i])
#' 	}
#' 
#' 
calcLvls <- function(M, n = 10, mode = "even",
	lambda = 1.0, base = 2,
	showHist = FALSE, ...) {

	if (mode == "even") {
		n <- as.integer(n)
		mn <- min(M)
		mx <- max(M)
		lower <- mn * 0.95 # shrink in a bit at both ends
		upper <- mx * 0.95
		lvs <- seq(lower, upper, length.out = n)
		if (length(lvs) == 0) stop("Levels calculation returned no levels")
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "exp") { # For use when the range is [-Inf...Inf]
		n <- as.integer(n)
		
		if (lambda == 0.0) stop("lambda cannot be zero")
		
		# Compute levels based on the most extreme value
				
		ref <- .findExtreme(M)
		lower <- 0.00001 # just above zero to avoid Inf
		# lvs <- seq(log(lower), log(ref), length.out = floor(n/2))
		lvs <- seq(log(lower), log(ref), length.out = floor(n/2))
		lvs <- exp(lvs*lambda)
		
		# Now scale back into the range of the data
		# Shrink inward a bit, can't show a contour at max/min value (well, maybe one can...)
		lvs2 <- lvs * ref*0.975/max(lvs)

		# Reflect through zero
		lvs <- sort(c(-1*lvs2, lvs2))
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "log") { # For use when the range is [-Inf...Inf]
		n <- as.integer(n)
		if (base <= 0L) stop("base must be > 0")
		
		# Compute levels based on the most extreme value

		ref <- .findExtreme(M)				
		X <- .getPN(M)
		lower <- 0.001 # just above zero to avoid Inf
		lvs <- seq(lower, ref, length.out = floor(n/2))
		lvs <- log(lvs, base)
		
		# Now scale back into the range of the data
		lvs <- abs(min(lvs)) + lvs
		sf <- diff(range(X))/diff(range(lvs))
		lvs <- lvs * sf
		lvs[1] <- 0.1 * ref # a little fudge to stay off zero & near the mirror value
		
		# Reflect through zero
		lvs <- sort(c(-1*lvs, lvs))
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "ecdf") {
		if ((any(n > 1)) | (any(n < 0))) stop("For ecdf mode, n must be a vector of values on [0...1]")
		Fn <- ecdf(sort(M))
		P <- Fn(sort(M))
		lv <- c()
		for (i in 1:length(n)) {
			tmp <- max(grep(n[i], P))
			lv <- c(lv, tmp)	
			}
		lvs <- sort(M)[lv]
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}

	if (mode == "posexp") { # For use when you just want (+)-ive
		lvs <- calcLvls(M = M, n = n, mode = "exp", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs > 0]
		if (length(lvs) == 0) stop("Levels calculation returned no levels")
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "negexp") { # For use when you just want (-)-ive
		lvs <- calcLvls(M = M, n = n, mode = "exp", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs < 0]
		if (length(lvs) == 0) stop("Levels calculation returned no levels")
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}

	if (mode == "poslog") { # For use when you just want (+)-ive
		lvs <- calcLvls(M = M, n = n, mode = "log", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs > 0]
		if (length(lvs) == 0) stop("Levels calculation returned no levels")
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}


	if (mode == "neglog") { # For use when you just want (-)-ive
		lvs <- calcLvls(M = M, n = n, mode = "log", lambda = lambda, base = base, ...)
		lvs <- lvs[lvs < 0]
		if (length(lvs) == 0) stop("Levels calculation returned no levels")
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}

	if (mode == "NMR") {
		lvs <- calcLvls(M = M, n = 48, mode = "exp", lambda = 0.25, base = base, ...)
		if (showHist) .sH(M, lvs, ...)
		return(lvs)
		}

	} # end of calcLvls
