#' Clean Up Dots Arguments to Pass Elsewhere
#'
#' @param args A list of args as produced by \code{as.list(match.call())[-1]}.
#'   These are the arguments as passed to the calling function.
#'
#' @param func The name of the function that will receive the cleaned arguments.
#'
#' @return A modified list of arguments.
#'
#'
# The following is needed because sometimes func = "readJDX"
#' @importFrom readJDX readJDX
#'
#' @noRd
#'
.cleanArgs2D <- function(args, func = NULL) {
  if (is.null(func)) stop("You must supply a reference function")

  f2S2DOformals <- names(formals(files2Spectra2DObject))
  funcFormals <- names(formals(func))

  # Basic procedure is to
  #   1. Remove the files2Spectra2DObject arguments
  #   2. Remove arguments that func cannot accept
  #      (these would be args intended for a function
  #       other than func)
  #   3. Override selected arguments (these will be assigned
  #      manually in files2Spectra2DObject)

  # Step 1
  args[f2S2DOformals] <- NULL

  # Step 2
  keep <- names(args) %in% funcFormals
  args[!keep] <- NULL # remove any remaining arguments

  # Step 3

  if (func == "read.table") {
    if ("file" %in% names(args)) args$file <- NULL
  }

  if (func == "readJDX") {
    if ("file" %in% names(args)) args$file <- NULL
  }

  if (func == "list.files") {
    if ("pattern" %in% names(args)) args$pattern <- NULL
    if ("full.names" %in% names(args)) args$full.names <- NULL
  }

  return(args)
}
