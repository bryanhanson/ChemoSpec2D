#'
#'
#' Ester Data Sets
#' 
#' This data is composed of simulated 300 MHz NMR COSY spectra.
#' The samples are methyl, ethyl, propyl, and butyl esters
#' of acetic, propanoic, butanoic, pentanoic and hexanoic acids.
#' Not all combinations of alcohols and acids are present.
#' There are 18 samples total.
#' 
#' 
#' @author Bryan A. Hanson, DePauw University.
#'
#' @name Esters
#'
#' @docType data
#'
#' @format The data is stored as a \code{\link{Spectra2D}} object.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#'
#' @source Created using various tools.  Contact the author for a script if
#' interested.
#'
#' @keywords datasets
#'
#' @examples
#' data(Esters)
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
NULL

