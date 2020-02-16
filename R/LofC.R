#'
#' Create a List of Colors
#'
#' When overlaying multiple 2D NMR spectra, one needs to supply a vector of colors for each spectrum contour,
#' as a list with length equal to the number of spectra to be plotted.
#' This is a convenience function which takes a vector of colors and copies it into a list, ready for
#' use  with \code{\link{plotSpectra2D}}.
#'
#' @param cols Character. A vector of color designations.
#'
#' @param nspec Integer. The number of spectra to be plotted.
#'
#' @param ncon Integer. The number of contour levels.
#'
#' @param mode Integer. \emph{How} to replicate the colors:
#'        \itemize{
#'          \item If \code{mode = 1L}, each list element in the return value is a copy of \code{cols}.
#'                Use this mode when you want to use varied colors for the contour levels. \code{length(cols)}
#'                must be the same as the number of contour levels passed to \code{\link{plotSpectra2D}},
#'                possibly via \code{\link{LofL}}.
#'          \item If \code{mode = 2L}, each list element in the return value is composed of a single color.
#'                Use this mode when you want each spectrum to be plotted in its own color. The first list element
#'                is \code{ncon} replicates of \code{cols[1]}, the second list element is
#'                \code{n} replicates of \code{cols[2]} etc. \code{length(cols)} must equal \code{nspec} in this mode.
#'        }
#' @return A list of length \code{nspec}, the number of spectra to be plotted; each entry is a vector of colors.
#'
#' @export
#'
#' @examples
#' mycols <- c("red", "green", "blue")
#' LofC(mycols, 1, 3, 1)
#' LofC(mycols, 3, 3, 2)
LofC <- function(cols, nspec = 1L, ncon = 1L, mode = 1L) {
  success <- FALSE
  ans <- list()

  if (mode == 1L) { # n list elements, each a copy of cols
    if (ncon != length(cols)) stop("LofC mode = 1 requires that length(cols) = ncon")
    for (i in 1:nspec) ans[[i]] <- cols
    success <- TRUE
  }

  if (mode == 2L) { # 1st list element is ncon reps of cols[1], 2nd ncon reps of cols[2] etc
    if (nspec != length(cols)) stop("LofC mode = 2 requires that length(cols) = nspec")
    for (i in 1:nspec) ans[[i]] <- rep(cols[i], ncon)
    success <- TRUE
  }

  if (success) {
    return(ans)
  }
  if (!success) stop("Sorry, could not honor color request")
}
