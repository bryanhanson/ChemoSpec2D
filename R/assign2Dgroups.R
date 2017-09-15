#'
#' Assign Group Membership for a Spectra2D Object
#' 
#' A utility function which looks for \code{gr.crit} in the spectra/file names
#' and assigns group membership. Warnings are given if there are spectra/file names
#' that don't match entries in \code{gr.crit} or there are entries in
#' \code{gr.crit} that don't match any spectra/file names.  An internal function, not
#' generally called by the user.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}} missing \code{spectra$groups}.
#' 
#' @param gr.crit As per \code{\link{files2Spectra2DObject}}.
#' 
#' @return A \emph{complete} object of S3 class \code{\link{Spectra2D}}.  This
#' function is the last internal step in creating a \code{Spectra2D} object.
#' Until this function has done its job, an object of class
#' \code{\link{Spectra2D}} will not pass checks as the assembly is not complete
#' (see \code{\link{chkSpectra2D}}).
#' 
#' @author Bryan A. Hanson, DePauw University.
#' 
#' @seealso \code{\link{files2Spectra2DObject}} for details;
#' \code{\link{sumGroups2D}} to see the outcome.
#' 
#' @references \url{https://github.com/bryanhanson/ChemoSpec2D}
#' 
#' @keywords utilities
#' 
#' @export assign2Dgroups
#'
#'
assign2Dgroups <- function(spectra, gr.crit = NULL) {

	msg <- "At least one spectra name did not correspond any entry in gr.crit and its group is thus NA"
    
	# Use the group criteria (gr.crit) to classify the samples

	spectra$groups <- rep(NA_character_, length(spectra$names))
	
	for (i in 1:length(gr.crit)) {
		which <- grep(gr.crit[i], spectra$names)
		if (length(which) == 0) {
			warning("There was no match for gr.crit value ", gr.crit[i], " among the file names.")
			}
		spectra$groups[which] <- gr.crit[i]
		}
	
	spectra$groups <- as.factor(spectra$groups)
	if (any(is.na(spectra$groups))) warning(msg)
		
	return(spectra)
	}

