#'
#'
#' Summarize the Group Parameters of a Spectra2D Object
#' 
#' This function summarizes the group membership and descriptive parameters of
#' a \code{\link{Spectra2D}} object.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}} whose group
#' membership information is desired.
#'
#' @return A data frame as follows.  Note that if there are groups with no
#' members (due to previous use of \code{\link{removeSample2D}}), these are
#' dropped.  \item{group}{The name of the group.} \item{no.}{The number in the
#' group.}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso To summarize the entire object, \code{\link{sumSpectra2D}}.
#'
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#'
#' @keywords utilities
#'
#' @export sumGroups2D
#'
sumGroups2D <- function(spectra){
		
	chkSpectra2D(spectra)

	gr.l <- levels(spectra$group)
	count <- length(gr.l)
	g.sum <- data.frame(group = NA, no. = NA)
	
	for (n in 1:count) {
		gi <- match(gr.l[n], spectra$groups) # find index 1st instance
		gr <- gr.l[n] # value of group
		no. <- length(which(gr == spectra$groups))
		g.sum <- rbind(g.sum, data.frame(group = gr, no. = no.))
		}
	g.sum <- g.sum[-1,]
	g.sum <- subset(g.sum, no. > 0)
	rownames(g.sum) <- c(1:nrow(g.sum))
	g.sum
	}
