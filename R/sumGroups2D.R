#'
#'
#' Summarize the Groups in a Spectra2D Object
#' 
#' This function summarizes the group membership and descriptive parameters of
#' a \code{\link{Spectra2D}} object.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}} whose group
#' membership summary is desired.
#'
#' @return A data frame as follows.
#'         \item{group}{The name of the group.}
#'         \item{no.}{The number in the group.}
#'         \item{color}{The color assigned to the group.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso To summarize the entire object, \code{\link{sumSpectra2D}}.
#'
#' @keywords utilities
#'
#' @export sumGroups2D
#'
#' @examples
#'
#' data(MUD1)
#' sumGroups2D(MUD1)
#'
sumGroups2D <- function(spectra){
		
	chkSpectra2D(spectra)

	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	g.sum <- data.frame(group = NA, no. = NA, color = NA)
	
	for (n in 1:count) {
		gi <- match(gr.l[n], spectra$groups) # find index 1st instance
		gr <- gr.l[n] # value of group
		gc <- spectra$colors[gi]
		no. <- length(which(gr == spectra$groups))
		g.sum <- rbind(g.sum, data.frame(group = gr, no. = no., color = gc))
		}
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0)
	rownames(g.sum) <- c(1:nrow(g.sum))
	g.sum
	}
