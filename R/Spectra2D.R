#'
#' Spectra2D Objects
#' 
#' In \code{ChemoSpec2D}, spectral data sets are stored in an S3 class called
#' \code{Spectra2D}, which contains a variety of information in addition to the
#' spectra themselves.  \code{Spectra2D} objects are created by
#' \code{\link{files2Spectra2DObject}}.
#' 
#' @section Structure: The structure of a \code{Spectra2D} object is a list of eight
#' elements and an attribute as follows:
#' 
#' \tabular{lll}{
#'   \emph{element} \tab \emph{type} \tab \emph{description}\cr
#'
#'   $F2 \tab num \tab A common frequency (or wavelength) axis corresponding \cr
#'       \tab \tab to the F2 dimension in NMR or the x axis more generally.\cr
#'       \tab \tab Must be sorted ascending.\cr
#'
#'   $F1 \tab num \tab A common frequency (or wavelength) axis corresponding \cr
#'       \tab \tab to the F1 dimension in NMR or the y axis more generally.\cr
#'       \tab \tab Must be sorted ascending.\cr
#'
#'   $data \tab num \tab A list of matrices.  Each matrix contains a 2D spectrum.\cr
#'         \tab \tab Each matrix should have \code{length(F1)} rows and \cr
#'         \tab \tab \code{length(F2)} columns. The low end of the F2\cr
#'         \tab \tab dimension is last column of the last row (lower right hand corner\cr
#'         \tab \tab as typically displayed).  The low end of the F1 dimension\cr
#'         \tab \tab is the last column of the first row (upper right hand corner).\cr
#'         \tab \tab In other words, the spectrum is stored as typically displayed.\cr
#'         \tab \tab The list of matrices, if named, should have the same names as\cr
#'         \tab \tab \code{names}.  However, this is not currently enforced.\cr
#' 
#'   $names \tab chr \tab The sample names for the spectra; length must be no. samples.\cr
#'
#'   $groups \tab factor \tab The group classification of the samples; length must be no. samples.\cr
#'
#'   $colors \tab character \tab Colors for plotting; length must be no. samples.\cr
#'           \tab \tab Colors correspond to groups.\cr
#'
#'   $units \tab chr \tab Three entries, the first giving the F2 (x) axis unit, the\cr
#'         \tab \tab second the F1 (y) axis unit, and the third the z axis unit,\cr
#'         \tab \tab usually some kind of intensity.\cr
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
#' @keywords classes
#'
#' @examples
#'
#' data(MUD1)
#' str(MUD1)
#' sumSpectra2D(MUD1)
#'

NULL
