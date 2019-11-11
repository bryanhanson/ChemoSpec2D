#'
#' Create a List of Colors
#'
#' When overlaying multiple 2D NMR spectra one needs to supply a vector of colors for each spectrum contour, as a list.
#' This is a convenience function which takes a vector of colors and copies it into a list, ready for
#' use  with \code{\link{plotSpectra2D}}.
#'
#' @param cols Character. A vector of color designations.
#'
#' @param n Integer. The number of times to replicate the colors.
#'
#' @param mode Integer. \emph{How} to replicate the colors:
#'        \itemize{
#'        \item If \code{mode = 1L}, a list of length \code{n}, where \code{n} is the number of spectra to
#'              be plotted. Each element a copy of \code{cols}.  This mode is probably most useful when
#'              \code{n = 1} (i.e. there is only one spectrum to be plotted, and you want to use custom colors).
#'        \item If \code{mode = 2L}, a list of \code{length(cols)}, which should correspond to the number of spectra
#'              to be plotted.  First list element is \code{n} replicates
#'              of \code{cols[i]}, second list element is \code{n} replicates of \code{cols[2]} etc.
#'              Use this mode when you want each spectrum to be plotted in its own color with \code{n} contours.
#'        }
#' @return A list of length \code{n}; each entry is a vector of colors.
#'
#' @export
#'
#' @examples
#' mycols <- c("red", "green", "blue")
#' LofC(mycols, 1, 1)
#' LofC(mycols, 5, 2)
#'
LofC <- function(cols, n = 1L, mode = 1L) {

  success <- FALSE
  ans <- list()
    
  if (mode == 1L) { # n list elements, each a copy of cols
  	for (i in 1:n) ans[[i]] <- cols
  	success <- TRUE
  }
    
  if (mode == 2L) { # 1st list element is n reps of cols[1], 2nd n reps of cols[2] etc
  	for (i in 1:length(cols)) ans[[i]] <- rep(cols[i], n)
  	success <- TRUE
  }

  if (success) return(ans)
  if (!success) stop("Sorry, could not honor color request")
}
