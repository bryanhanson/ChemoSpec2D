#'
#' Construct Default Color Scale
#'
#' @param n Integer.  The number of colors will be \code{2n -2}.
#'
#' @param palette Character.  Currently only \code{"redblue"}.
#'
#' @param mode Character.  One of \code{"normal"}, \code{"protan"}, \code{"deutan"} or \code{"tritan"}.
#'
#' @return A vector of colors.
#'
#' @export
#' @noRd
#' @importFrom colorspace sequential_hcl deutan protan tritan
#' @examples
#' pie(rep(1, 8), col = .createScale())
#'

.createScale <- function(n = 5, palette = "redblue", mode = "normal") {
  # Original version:
  # blue/low -> red/high, anchored at zero (index 5, a shade of green)
  # view with:
  # pie(rep(1, 9), col = cscale)
  # col1 <- rev(rainbow(5, start = 0.0, end = 0.25))
  # col2 <- rev(rainbow(4, start = 0.45, end = 0.66))
  # cscale <- c(col2, col1)
  
  if (palette == "redblue") {
    ch <- 110 # chroma
    ll <- 30 # lower limit of luminance
    # reds
    mycR <- sequential_hcl(n, h = 5, c = ch, l = c(ll, 100), power = 1.5, fixup = TRUE, rev = FALSE)
    # blues
    mycB <- sequential_hcl(n, h = 260, c = ch, l = c(ll, 100), power = 1.5, fixup = TRUE, rev = TRUE)
    # combine
    myc <- rev(c(mycR[-n], mycB[-1]))
    if (mode == "deutan") myc <- deutan(myc)
    if (mode == "protan") myc <- protan(myc)
    if (mode == "tritan") myc <- tritan(myc)
  }
  
  myc
} # end of .createScale


