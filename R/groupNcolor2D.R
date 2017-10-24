#'
#' Assign Group Membership and Colors for a Spectra2D Object
#' 
#' A utility function which looks for \code{gr.crit} in the spectra/file names
#' and assigns group membership. Warnings are given if there are spectra/file names
#' that don't match entries in \code{gr.crit} or there are entries in
#' \code{gr.crit} that don't match any spectra/file names.
#' Colors are also assigned.  An internal function, not
#' generally called by the user.
#' 
#' @param spectra An \emph{incomplete} object of S3 class \code{\link{Spectra2D}} which is missing \code{spectra$groups} and \code{spectra$colors}.
#' 
#' @param gr.crit As per \code{\link{files2Spectra2DObject}}.
#' 
#' @param gr.cols As per \code{\link{files2Spectra2DObject}}.
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
#' @keywords utilities
#' 
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#'
groupNcolor2D <- function(spectra, gr.crit = NULL, gr.cols = "auto") {

	msg1 <- "At least one file name did not correspond any entry in gr.crit and its group is thus NA"
    msg2 <- "More groups than colors, colors will be recycled.\n  Redefine groups or specify colors manually."
    
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
	if (any(is.na(spectra$groups))) warning(msg1)

	# Assign each group a color for plotting later

	spectra$colors <- rep(NA_character_, length(spectra$names))
	
	if (identical(gr.cols[1], "auto")) {
		if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
			stop("You need to install package RColorBrewer or supply the colors yourself")
			}

		if (length(gr.crit) > 8) warning(msg2)
		cscols <- brewer.pal(8, "Set1") # 9 colors in Set1, only using 8
		gr.cols <- cscols[1:length(gr.crit)]

		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$names)
			spectra$colors[which] <- gr.cols[i]
			}
		}
		
	if (!identical(gr.cols[1], "auto")) {
		if (length(gr.cols) != length(gr.crit)) stop("Length of gr.cols and gr.crit did not match")
		for (i in 1:length(gr.crit)) {
			which <- grep(gr.crit[i], spectra$groups)
			spectra$colors[which] <- gr.cols[i]
			}
		}
		
	return(spectra)
	}

