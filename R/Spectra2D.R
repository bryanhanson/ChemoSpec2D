#'
#'
#' Spectra2D Objects
#' 
#' In \code{ChemoSpec2D}, spectral data sets are stored in an S3 class called
#' \code{Spectra2D}, which contains a variety of information in addition to the
#' spectra themselves.  \code{Spectra2D} objects are created by
#' ???.
#' 
#' 
#' @section Structure: The structure of a \code{Spectra2D} object is a list of ???
#' elements and an attribute as follows:
#' 
#' \tabular{lll}{
#'   \emph{element} \tab \emph{type} \tab \emph{description}\cr
#'
#'   $F2 \tab num \tab A common frequency (or wavelength) axis for all the
#'   spectra, corresponding to the F2 dimension in NMR.\cr
#'
#'   $F1 \tab num \tab A common frequency (or wavelength) axis
#'   corresponding to the F1 dimension in NMR.\cr
#'
#'   $data \tab num \tab A list where each element is a 2D spectrum, in the
#'   form of a matrix with dimensions ??? \cr
#' 
#'   $names \tab chr \tab The sample names for the spectra; length must be no.
#'   samples.\cr
#'
#'   $groups \tab Factor \tab The group classification of the
#'   samples; length must be no. samples.\cr
#'
#'   $unit \tab chr \tab Two entries, the
#'   first giving the x axis unit, the second the y axis unit.\cr
#'
#'   $desc \tab chr \tab A character string describing the data set.
#'   This appears on plots and therefore \cr \tab \tab should probably
#'   be kept to 40 characters or less.\cr
#' 
#'   - attr \tab chr "Spectra2D" \tab The S3 class designation.\cr
#' }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{sumSpectra2D}} to summarize a \code{Spectra2D} object.
#' \code{\link{sumGroups}} to summarize group membership of a \code{Spectra2D}
#' object. \code{\link{chkSpectra2D}} to verify the integrity of a
#' \code{Spectra2D} object.
#'
#' @name Spectra2D
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#'
#' @keywords classes
NULL
