### Draw the scale/legend

#'
#' @export
#' @noRd
#' @importFrom graphics plot.new mtext rect
#'

.drawScale <- function(cscale, orient) {

	nc <- length(cscale)
	
	if (orient == "horizontal") {
	  plot(1:nc, rep(1.0, nc), type = "n",
		yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
	  for (i in 1:nc) {
		rect(i-0.5, 0.5, i+0.5, 1.5, border = NA, col = cscale[i])
	  }	
	} # end of orient == "horizontal"

	if (orient == "vertical") {
      plot.new()
      op <- par(no.readonly = TRUE) # save to restore later (must call before layout)
      par(mai = c(0.75, 3.4, 0.75, 3.4))

	  plot(rep(1.0, nc), 1:nc, type = "n",
		yaxt = "n", xaxt = "n", main = "", xlab = "", ylab = "")
	  for (i in 1:nc) {
		rect(0.5, i-0.5, 1.5, i+0.5, border = NA, col = cscale[i])
	  }
	  mtext("low", side = 1)
	  mtext("high", side = 3)
      on.exit(par(op)) # restore original values

	} # end of orient == "vertical"
	
}
