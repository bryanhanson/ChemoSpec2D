#'
#' @noRd
#'
.onAttach <- function(libname, pkgname) {
  options(ChemoSpecGraphics = "ggplot2")
  chkReqGraphicsPkgs("ggplot2")

  packageStartupMessage("\nAs of version 6, ChemoSpec2D offers new graphics output options")
  packageStartupMessage("\nFunctions plotScores and plotScree will work with the new options")
  packageStartupMessage("For details, please see ?GraphicsOptions")
  packageStartupMessage("\nThe ChemoSpec graphics option is set to 'ggplot2'")
  packageStartupMessage("To change it, do\n\toptions(ChemoSpecGraphics = 'option'),\n\twhere 'option' is one of 'base' or 'ggplot2' or'plotly'.")
}