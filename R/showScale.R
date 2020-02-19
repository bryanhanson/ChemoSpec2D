#'
#' Display a pdf Version of the Contour Scale
#'
#' This function opens the files containing the contour scale in an appropriate viewer.
#' One file has a white background and the other a black background.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' showScale()
#' }
#'
showScale <- function() {
  # Based on stackoverflow.com/a/37505297/633251
  path <- system.file("extdata", "scale_bg_white.pdf", package = "ChemoSpec2D")
  system(paste0('open "', path, '"'))
  path <- system.file("extdata", "scale_bg_black.pdf", package = "ChemoSpec2D")
  system(paste0('open "', path, '"'))
}
