### Construct default color scale

#'
#' @export
#' @noRd
#'

.createScale <- function() {
  # blue/low -> red/high, anchored at zero (index 5, a shade of green)
  # view with:
  # pie(rep(1, 9), col = cscale)
	col1 <- rev(rainbow(5, start = 0.0, end = 0.25))
	col2 <- rev(rainbow(4, start = 0.45, end = 0.66))
	cscale <- c(col2, col1)	
}
