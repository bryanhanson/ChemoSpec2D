#'
#' Create a List of Colors
#'
#' When overlaying multiple 2D NMR spectra one needs to supply a vector of colors for each spectrum contour, as a list.
#' This is a convenience function which takes a vector of colors and copies it into a list, ready for
#' use  with \code{\link{plotSpectra2D}}.
#'
#' @param cols Integer or Character.  If integer, \code{n} colors will be chosen from the standard color
#'        scheme used by \code{ChemoSpec2D}.  Otherwise a character vector of color designations.
#'
#' @param n Integer. The number of times to replicate the colors.
#'
#' @param mode Integer. If \code{mode = 1L}, the values in \code{cols} will be replicated in each list entry.
#'        If \code{mode = 2L}, the length of \code{cols} must equal \code{n} and the first list entry will 
#'        be \code{n} values of the first color, the second list entry will be \code{n} values of the second color etc.
#'
#' @return A list of length \code{n}; each entry is a vector of colors.
#'
#' @seeAlso For examples, see \code{\link{LoL}}.
#'
#' @export
#'
#' @examples
#' LofC(5, 1)
#' mycols <- c("red", "green", "blue")
#' LofC(mycols, 1)
#' LofC(mycols, 3, 2)

LofC <- function(cols, n = 1L, mode = 1L) {

  success <- FALSE
  ans <- list()
  
  # user requests a certain number of standard colors which will be used in the next blocks
  if (is.numeric(cols)) cols <- .createScale(as.integer(cols))
  
  if (is.character(cols) & (mode == 1L)) { # each list element is cols
  	for (i in 1:n) ans[[i]] <- cols
  	success <- TRUE
  }
  
  if (is.character(cols) & (mode == 2L) & (length(cols) == n)) { # list element 1 is cols[1] etc
    for (i in 1:n) ans[[i]] <- rep(cols[i], n)
  	success <- TRUE
  }
  
  if (success) return(ans)
  if (!success) stop("Sorry, could not honor color request")
}
