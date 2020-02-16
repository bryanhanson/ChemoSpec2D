#'
#' Map colors to go with each contour level
#'
#' This function should accept arbitrary vectors of levels and map them
#' onto the reference color scale.  The color map should maintain symmetry present
#' in the levels so -4.1 should give a color symmetric to +4.1
#' Mapping should be relative to the range of the entire Spectra2D object, not just
#' a single spectrum.  That way different spectra in a set can be compared directly.
#' Note that if one is plotting a loading, then the computation of range may be greatly
#' skewed by the loadings entry.  For now, live with it, levels can always be provided.
#'
#' @export
#' @noRd
#'
.mapColors <- function(spectra, lvls) {
  .chkArgs(mode = 21L)

  cscale <- .createScale()
  drange <- range(spectra$data, na.rm = TRUE) # some data sets have NAs
  drange <- .symAroundZero(drange)
  if ((max(lvls) > drange[2]) | (min(lvls) < drange[1])) warning("Some levels are outside the range of the data")
  refscale <- seq(drange[1], drange[2], length.out = length(cscale) + 2)
  myc <- na.omit(cscale[findInterval(lvls, refscale, all.inside = TRUE)])
  return(myc)
}
