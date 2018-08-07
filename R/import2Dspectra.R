#'
#'
#' Import 2D Spectroscopic Data
#' 
#' This function imports a single file (for instance, a csv) containing a 2D
#' spectroscopic data set.  An internal function, not
#' generally called by the user.
#'
#' If \code{fmt = "YXZ"} it is assumed that the columns contain Y, X and Z
#' values describing the data.
#' 
#' @param file Character string giving the path to a file containing a 2D spectrum.
#'
#' @param fmt Character string giving the format to use (currently limited to \code{"YXZ"}).
#'
#' @param npx Integer giving the number of data points in the F2 (x) dimension.
#'
#' @param ... Parameters to be passed to \code{\link{read.table}}.
#'
#' @return A list with 3 elements:
#' \itemize{
#'   \item A matrix of the z values.  The no. of rows = \code{npx} and the no. of columns
#'         follows from the size of the imported data.
#'   \item A vector giving the F2 (x) values.
#'   \item A vector giving the F1 (y) values.
#'  }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords import
#' 
#' @importFrom utils read.table
#'
import2Dspectra <- function(file, fmt, npx, ...) {
	
  valid <- FALSE
  
  if (fmt == "YXZ") {
  	valid <- TRUE
    raw <- read.table(file, ...)
    M <- matrix(raw[,3], nrow = npx, byrow = TRUE)
    F2 <- unique(raw[,1])
    F1 <- unique(raw[,2])
    ans <- list(M = t(M), F2 = F2, F1 = F1) # transpose so F2 is along x axis
    return(ans) 
  } # end of fmt = "XYZ"
  
  if (!valid) stop("fmt not recognized")
}
