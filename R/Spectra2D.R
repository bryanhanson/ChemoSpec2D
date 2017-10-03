#'
#' Spectra2D Objects
#' 
#' In \code{ChemoSpec2D}, spectral data sets are stored in an S3 class called
#' \code{Spectra2D}, which contains a variety of information in addition to the
#' spectra themselves.  \code{Spectra2D} objects are created by
#' \code{\link{files2Spectra2DObject}}.
#' 
#' @section Structure: The structure of a \code{Spectra2D} object is a list of seven
#' elements and an attribute as follows:
#' 
#' \tabular{lll}{
#'   \emph{element} \tab \emph{type} \tab \emph{description}\cr
#'
#'   $F2 \tab num \tab A common frequency (or wavelength) axis corresponding
#'   \cr \tab \tab to the F2 dimension in NMR or the x axis more generally.\cr
#'
#'   $F1 \tab num \tab A common frequency (or wavelength) axis corresponding
#'   \cr \tab \tab to the F1 dimension in NMR or the y axis more generally.\cr
#'
#'   $data \tab num \tab A list of matrices.  Each matrix contains a 2D spectrum.
#'   \cr \tab \tab Each matrix should have dimensions \code{length(F2) x length(F1)}.\cr
#' 
#'   $names \tab chr \tab The sample names for the spectra; length must be no.
#'   samples.\cr
#'
#'   $groups \tab factor \tab The group classification of the
#'   samples; length must be no. samples.\cr
#'
#'   $unit \tab chr \tab Three entries, the
#'   first giving the F2 (x) axis unit, the second the\cr \tab \tab F1 (y) axis unit,
#'   and the third the z axis unit,\cr \tab \tab usually some kind of intensity.\cr
#'
#'   $desc \tab chr \tab A character string describing the data set.\cr
#' 
#'   - attr \tab chr \tab "Spectra2D" The S3 class designation.\cr
#' }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{sumSpectra2D}} to summarize a \code{Spectra2D} object.
#' \code{\link{sumGroups2D}} to summarize group membership of a \code{Spectra2D}
#' object. \code{\link{chkSpectra2D}} to verify the integrity of a
#' \code{Spectra2D} object.
#'
#' @name Spectra2D
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#'
#' @keywords classes
#'
#'

NULL
