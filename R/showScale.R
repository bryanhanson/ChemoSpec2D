#'
#' Display a pdf Version of the Contour Scale
#'
#' This function opens the file scale.pdf in an appropriate viewer.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' showScale()
#' }

showScale <- function() {
  # Based on stackoverflow.com/a/37505297/633251
  path <- system.file("extdata", "scale.pdf", package = "ChemoSpec2D")
  system(paste0('open "', path, '"'))	
}

