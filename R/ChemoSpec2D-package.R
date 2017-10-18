#'
#'
#'
#' Exploratory Chemometrics for 2D Spectroscopy
#' 
#' A collection of functions for working with 2D spectra,
#' developed with NMR in mind, but other types of 2D spectroscopy may also
#' be analyzed. ChemoSpec2D takes many of its cues from ChemoSpec
#' and tries to create consistent graphical output and be very user friendly.
#' Tools to plot 2D spectra and carry out PARAFAC are the main features. 
#' 
#' @name ChemoSpec2D-package
#' 
#' @aliases ChemoSpec2D-package ChemoSpec2D
#' 
#' @docType package
#' 
#' @author Bryan A. Hanson.
#' 
#' Maintainer: Bryan A. Hanson \email{hanson@@depauw.edu}
#' 
#' @keywords package
#'
#' @examples
#' if (!requireNamespace("ChemoSpec2Ddata", quietly = TRUE)) {
#'   stop("You must install ChemoSpec2Ddata to run this example")
#' }
#'
#' # ChemoSpec2Ddata can be installed following the directions here:
#' # https://github.com/bryanhanson/ChemoSpec2Ddata
#'
#' data("Esters", package = "ChemoSpec2Ddata")
#' sumSpectra2D(Esters)
#' sumGroups2D(Esters)
#' chkSpectra2D(Esters, confirm = TRUE)
#'
#' Esters2 <- removeGroup2D(Esters, "butyl") # remove the butyl esters
#' sumGroups2D(Esters2)
#'
#' Esters3 <- removeSample2D(Esters, "Pro") # remove the propanoates
#' sumSpectra2D(Esters3)
#'
#' Esters4 <- normSpectra2D(Esters)
#'
#' plotSpectra2D(Esters)
#'
NULL


