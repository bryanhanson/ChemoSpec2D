#'
#'
#' Made Up 2D NMR-Like Data Sets
#' 
#' Made Up Data that resemble a simple, HSQC-like 2D NMR data set.  Lean, low resolution 
#' and designed primarily to check graphics and test functions. As this is made up
#' data, there is no underlying structure and therefore one should NOT try to interpret
#' the output of \code{miaSpectra2D} or \code{pfacSpectra2D} run on this data.
#' Real 2D NMR data sets are too large to be included in the package.
#' 
#' \code{MUD1} is composed of 10 samples in two groups.
#' 
#' @author Bryan A. Hanson, DePauw University.
#'
#' @name MUD1
#'
#' @docType data
#'
#' @format The data are stored as a \code{\link{Spectra2D}} object.
#'
#' @source Created from scratch. Contact the author for a script if interested.
#'
#' @keywords datasets
#'
#' @examples
#'
#' data(MUD1)
#' sumSpectra2D(MUD1)
#'
NULL

