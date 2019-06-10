#'
#' Shift an Array and Backfill with Zeros, Random Numbers or Noise
#'
#' \emph{The first dimension of the array is not affected.}  If using \code{rnorm} to fill
#' the user should \code{set.seed()}. Shifts are defined as:
#' \itemize{
#'   \item (+) x-shift - shift right: trim right, fill left
#' 	 \item (-) x-shift - shift left: trim left, fill right
#' 	 \item (+) y-shift - shift up: trim top, fill bottom
#' 	 \item (-) y-shift - shift down: trim bottom, fill top
#' }
#'
#' @param A An array.
#'
#' @param xshift An integer giving the amount to shift the array in the x-direction.
#'
#' @param yshift As for x-shift.
#'
#' @param NS An array of noise surfaces, produced by \code{.noiseSurface}.
#'
#' @param fill One of:
#' \itemize{
#'   \item \code{"zero"}: empty spaces are filled with zeros.
#'   \item \code{"rnorm"}: empty spaces are filled via \code{rnorm(x, 0.0, 0.1)}.
#'   \item \code{"noise"}: empty spaces are filled with noise from the original 2D spectra.
#'     You must supply \code{NS}.
#' }
#'
#' @importFrom stats rnorm
#' @export
#' @noRd
#'

.shiftArray <- function(A,
  xshift = 0, yshift = 0,
  fill = "zero", NS = NULL) {
  
  xshift <- as.integer(xshift)
  yshift <- as.integer(yshift)
  
  if ((xshift == 0L) & (yshift == 0L)) stop(".shiftArray received shift instructions of 0,0")
    
  if (abs(xshift) >= (dim(A)[3]-2)) stop("Cannot shift matrix that far in x-direction")
  if (abs(yshift) >= (dim(A)[2]-2)) stop("Cannot shift matrix that far in y-direction")
  
  # Step 1. Set up an array to contain the shifted results
  
  if ((fill == "zero") | (fill == "rnorm")) {
    ns <- dim(A)[1]
    nr <- dim(A)[2]
    nc <- dim(A)[3]
    if (fill == "zero") Ash <- array(0.0, dim = c(ns, nr, nc)) 
    if (fill == "rnorm") Ash <- array(rnorm(ns*nr*nc, 0.0, 0.1), dim = c(ns, nr, nc))
  }
  
  if (fill == "noise") {
    if (is.null(NS)) stop("For fill = 'noise' you must provide a noise array")   
    if (isFALSE(all.equal(dim(A), dim(NS)))) stop("Input array and noise array did not have the same dimensions")
    ns <- dim(NS)[1]
    nr <- dim(NS)[2]
    nc <- dim(NS)[3]
    Ash <- NS
  }
  
  # Step 2. Trim the original array (trim = remove rows & cols from edges)
  
  trimLeft <- 1:abs(xshift)
  trimRight <- (nc - abs(xshift) + 1):nc
  trimTop <- 1:abs(yshift)
  trimBot <- (nr - abs(yshift) + 1):nr
  
  # trim on one dimension
  if ((xshift > 0) & (yshift == 0)) Atmp <- A[,, -trimRight, drop = FALSE]
  if ((xshift < 0) & (yshift == 0)) Atmp <- A[,, -trimLeft, drop = FALSE]
  if ((xshift == 0) & (yshift > 0)) Atmp <- A[, -trimTop,, drop = FALSE]
  if ((xshift == 0) & (yshift < 0)) Atmp <- A[, -trimBot,, drop = FALSE]

  # trim on two dimensions
  if ((xshift > 0) & (yshift > 0)) Atmp <- A[, -trimTop, -trimRight, drop = FALSE]
  if ((xshift > 0) & (yshift < 0)) Atmp <- A[, -trimBot, -trimRight, drop = FALSE]
  if ((xshift < 0) & (yshift > 0)) Atmp <- A[, -trimTop, -trimLeft, drop = FALSE]
  if ((xshift < 0) & (yshift < 0)) Atmp <- A[,- trimBot, -trimLeft, drop = FALSE]
  
  # Step 3. Place Atmp w/i Ash
  
  upper <- 1:dim(Atmp)[2]
  lower <- (dim(Ash)[2] - dim(Atmp)[2] + 1):dim(Ash)[2]
  left <- 1:dim(Atmp)[3]
  right <- (dim(Ash)[3] - dim(Atmp)[3] + 1):dim(Ash)[3]
  
  # fill on one dimension
  if ((xshift > 0) & (yshift == 0)) Ash[,, right] <- Atmp
  if ((xshift < 0) & (yshift == 0)) Ash[,, left] <- Atmp
  if ((xshift == 0) & (yshift > 0)) Ash[, upper,] <- Atmp
  if ((xshift == 0) & (yshift < 0)) Ash[, lower,] <- Atmp

  # fill on two dimensions
  if ((xshift > 0) & (yshift > 0)) Ash[, upper, right] <- Atmp
  if ((xshift > 0) & (yshift < 0)) Ash[, lower, right] <- Atmp
  if ((xshift < 0) & (yshift > 0)) Ash[, upper, left] <- Atmp
  if ((xshift < 0) & (yshift < 0)) Ash[, lower, left] <- Atmp
  
  Ash
  
}
