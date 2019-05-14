#'
#'
#' Made Up 2D NMR-Like Data Sets
#' 
#' Made Up Data that resemble simple, HSQC-like 2D NMR data sets.  Lean, low resolution 
#' and designed primarily to check graphics and test functions. \pkg{As this is made up
#' data, there is no underlying tri-linear structure and therefore one should NOT try to interpret
#' the output of \code{miaSpectra2D} or \code{pfacSpectra2D} run on this data}.
#' \itemize{
#'
#'   \item \code{MUD1} is intended to test and demonstrate data reduction functions.  The HSQC-like data
#' is derived from the 1H and 13C spectra of 3-methyl-1-butanol and the corresponding ethyl ether,
#' idealized slightly for simplicity. There are 10 spectra.  Samples 1 and 6 are the pure compounds.
#' The other samples are mixtures of the two compounds.
#'
#' \item \code{MUD2} is intended to test and demonstrate alignment algorithms.  The HSQC-like data
#' is derived from the 1H and 13C spectra of 3-methyl-1-butanol, idealized slightly for
#' simplicity. There are 10 spectra.  The first one is "correct" and shifts on one or both
#' dimensions have been introduced in the other samples.
#' }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @name MUD
#' @alias MUD1
#' @alias MUD2
#'
#' @docType data
#'
#' @format The data are stored as a \code{\link{Spectra2D}} object.
#'
#' @source Created from scratch. Contact the author for a script if interested.
#'
#' @keywords datasets
#'
#' @seeAlso These data sets are used in the examples of many functions.
#'
NULL

