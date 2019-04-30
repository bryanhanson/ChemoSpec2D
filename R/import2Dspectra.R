#'
#'
#' Import 2D Spectroscopic Data
#' 
#' This function imports a single file (for instance, a csv) containing a 2D
#' spectroscopic data set.  The current version handles various types of 
#' ASCII text files as well as a few other types.
#' This function is called by \code{files2Spectra2DObject}
#' and is exported and documented to assist in developing new format codes.
#'
#' @param file Character string giving the path to a file containing a 2D spectrum.
#'
#' @param fmt Character string giving the format code to use.
#'
#' @param nF2 Integer giving the number of data points in the F2 (x) dimension.
#'
#' @param ... Parameters to be passed to \code{\link{read.table}}.
#'
#' @section ASCII Format Codes:
#' ASCII format codes are constructed in two parts separated by a hyphen.  The first part gives
#' the order of the columns in the file, e.g. F2F1Z means the first column has the F2 values,
#' the second column has the F1 values and the third the intensities.  The second part of the
#' format code relates to the order of the rows, i.e. which column varies fastest and in what direction.
#' These codes are best understood in relation to how the data is stored internally in a matrix.
#' The internal matrix is organized exactly as the data appears on the screen, with F2 decreasing
#' left-to-right, and F1 increasing top-to-bottom. There are four possibilities:
#' \itemize{
#'   \item Data in the file consists of F2 slices starting at the bottom (max F1).  Use \code{F2decF1}.
#'   \item Data in the file consists of F2 slices starting at the top (min F1).  Use \code{F2incF1}.
#'   \item Data in the file consists of F1 slices starting on the left (max F2).  Use \code{F1decF2}.
#'   \item Data in the file consists of F1 slices starting on the right (max F1).  Use \code{F1incF2}.
#' }
#'
#' @section Other Format Codes:
#' Here are some other format codes you can use:
#' \itemize{
#'   \item \code{SimpleM}.  Imports simple matrices composed of z values.  The F2 and F1 values
#'         are taken from the dimension of the matrix.  After import, you will have to manually
#'         fix the F2 and F1 values.  You may also have to transpose the matrices manually, or
#'         perhaps invert the order of the rows or columns.
#' }

#'
#' @return A list with 3 elements:
#' \itemize{
#'   \item A matrix of the z values.  The no. of rows = \code{nF2} and the no. of columns
#'         follows from the size of the imported data.
#'   \item A vector giving the F2 (x) values.
#'   \item A vector giving the F1 (y) values.
#'  }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords import
#' 
#' @export
#'
#' @importFrom utils read.table
#'
import2Dspectra <- function(file, fmt, nF2, ...) {
	
  valid <- FALSE
  
  if (fmt == "SimpleM") {
  	valid <- TRUE
    raw <- read.table(file, ...)
    M <- as.matrix(raw)
    F2 <- as.numeric(1:ncol(M))
    F1 <- as.numeric(1:nrow(M))
    ans <- list(M = M, F2 = F2, F1 = F1)
    return(ans) 
  } # end of fmt = "SimpleM"

    if (fmt == "YXZ") {
  	valid <- TRUE
    raw <- read.table(file, ...)
    M <- matrix(raw[,3], nrow = nF2, byrow = TRUE)
    F2 <- sort(unique(raw[,1]))
    F1 <- sort(unique(raw[,2]))
    ans <- list(M = t(M), F2 = F2, F1 = F1) # transpose so F2 is along x axis
    return(ans) 
  } # end of fmt = "XYZ"

  if (fmt == "F2F1Z-F2decF1") {
  	valid <- TRUE
    raw <- read.table(file, ...)
    M <- matrix(raw[,3], nrow = nrow(raw)/nF2, byrow = TRUE)
    M <- M[nrow(M):1,] # reflect around horizontal axis as last row was first in file
    F2 <- sort(unique(raw[,1]))
    F1 <- sort(unique(raw[,2]))
    ans <- list(M = M, F2 = F2, F1 = F1)
    return(ans) 
  } # end of fmt = "F2F1Z-F2decF1"
  
  if (!valid) stop("fmt not recognized")
}
