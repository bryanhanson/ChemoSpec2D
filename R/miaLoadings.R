#'
#' Plot Loadings from a MIA of a Spectra2D Object
#' 
#' Plots loadings from a MIA (multivariate image analysis) of a
#' \code{\link{Spectra2D}} object.  The loadings are presented as
#' a 2D pseudo-spectrum with dimensions F1 x F2.
#' A reference spectrum may also be drawn.
#' 
#' @param spectra An object of S3 class \code{\link{Spectra2D}}.
#'
#' @param mia An object of class \code{mia} obtained by running
#' \code{\link{miaSpectra2D}}.
#'
#' @param load An integer specifying the loading to plot.
#'
#' @param ref An integer giving the spectrum in \code{spectra} to use
#'        as a reference spectrum, which is plotted behind the loadings.
#'        Defaults to \code{NULL} which does not plot a reference spectrum.
#'
#' @param ref_lvls A vector specifying the levels at which to compute contours
#'        for the reference spectrum.
#'        If \code{NULL}, values are computed using \code{calcLvls}.
#'
#' @param load_lvls A vector specifying the contour levels
#'        for the loadings pseudo-spectrum.
#'        If \code{NULL}, values are computed using \code{calcLvls}.
#'
#' @param ref_cols A vector specifying the colors for the contours in the reference
#'        spectrum. If \code{NULL}, set to gray.
#'
#' @param load_cols A vector specifying the colors for the contours in the loading spectrum.
#'        If \code{NULL}, defaults to a scheme of values
#'        running from blue (low) to red (high), centered on green (zero).
#'
#' @param plot Logical.  Shall a plot be made?  Plotting large data sets can be slow.
#'        Run the function with \code{plot = FALSE}, then use \code{\link{inspectLvls}}
#'        to figure out desirable levels, then create the plot.
#'
#' @param \dots Additional parameters to be passed to plotting functions.  For instance
#'        \code{showGrid = TRUE}.
#'
#' @return The modified \code{Spectra2D} object is returned invisibly.
#' The loadings matrix will be appended.  Side effect is a plot.
#'
#' @section Scale:
#' You can view the color scale for the plot via \code{\link{showScale}}.
#'
#' @section Levels & Colors:
#' The number of levels and colors must match, and they are used one for one.
#' If you provide \code{n} colors, and no levels, the automatic calculation of
#' levels may return a number of levels other than \code{n}, in which case the
#' function will override your colors and
#' assign new colors for the number of levels it computed (with a message).  To get
#' exactly what you want, specify both levels and colors in equal numbers.  Function
#' \code{\link{inspectLvls}} can help you choose appropriate levels.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @seealso Please see \code{\link{miaSpectra2D}} for examples.
#' 
#' @export
#'
miaLoadings <- function(spectra, mia,
  load = 1, ref = NULL,
  load_lvls = NULL, ref_lvls = NULL,
  load_cols = NULL, ref_cols = NULL, 
  plot = TRUE, ...) {
	
  .chkArgs(mode = 22L)
  
  if (length(load) != 1L) stop("Please supply a single loading")
  if (load > ncol(mia$C)) stop("Requested load does not exist")
  
  if (!is.null(ref)) {
  	if (length(ref) != 1L) stop("Please supply a single ref value")
  }
    
  chkSpectra(spectra)

  # Skip the computation if the requested loading has already been computed.
  # This is essential for large data sets which occupy a lot of memory which slows
  # down the contour calculations.  Go straight to plotting.
  
  pat <- paste("Loading", load, sep = "_")
  Ldone <- grep(pat, spectra$names)
  msg <- "Loading was present but no plot was requested; nothing to do"
  if ((length(Ldone) == 1L) & (plot == FALSE)) stop(msg)
  if (length(Ldone) == 0) { # loading was not found, compute it
  
	  # Computation per Geldadi & Grahn pg 124
	  # Stack each spectrum into a single "column"
	  ns <- length(spectra$names)
	  nF1 <- length(spectra$F1)
	  nF2 <- length(spectra$F2)
	  M1 <- matrix(NA_real_, ncol = ns, nrow = nF1 * nF2)
	  for (i in 1:ns) { M1[,i] <- as.vector(spectra$data[[i]]) }
	
	  # Compute loading matrix
	  L <- M1 %*% mia$C[, load]
	  # L <- L[nrow(L):1,ncol(L):1] # why was this here?  it's wrong
	  
	  # Unstack to the pseudospectrum
	  M2 <- matrix(L, ncol = nF2, nrow = nF1)
	 
	  # Update spectra object to include loading matrix
	  spectra$data[[ns + 1]] <- M2
	  spectra$names[ns + 1] <- paste("Loading", load, sep = "_")
	  Ldone <- ns + 1 # update for use in call to .plotEngine
	  spectra$groups <- as.factor(c(spectra$groups, "loadings"))
	  spectra$colors[ns + 1] <- "black"
	  chkSpectra(spectra)
	  
  } # end of computing the loading
  
  if (plot) {
  	
  ### Prep & send to plotEngine
	  # .plotEngine expects a Spectra2D object and lvls and cols as lists
	    
	  # Configure levels
	  # Note that ref is plotted first if at all (see call to .plotEngine below)
	  # See R Inferno 8.1.55 about setting list components to NULL (dont' do it)
	  if (is.null(ref)) { # only showing loadings
	    if (!is.null(load_lvls)) lvls <- list(load_lvls)
	    if (is.null(load_lvls)) lvls <- list(NULL)
	  }
	  
	  if (!is.null(ref)) { # showing loadings and reference spectrum
	    lvls <- vector("list", 2)  # intializes to NULL, NULL
	    if (!is.null(ref_lvls)) lvls[[1]] <- ref_lvls
	    if (!is.null(load_lvls)) lvls[[2]] <- load_lvls	
	  }
	    
	  # Configure colors
	
	  if (is.null(ref)) { # only showing loadings
	    if (!is.null(load_cols)) cols <- list(load_cols)
	    if (is.null(load_cols)) cols <- list(NULL)
	  }
	  
	  if (!is.null(ref)) { # showing loadings and reference spectrum
	    cols <- vector("list", 2)  # intializes to NULL, NULL
	    if (!is.null(ref_cols)) cols[[1]] <- ref_cols
	    if (!is.null(load_cols)) cols[[2]] <- load_cols	
	  }
	    
	  op <- par(no.readonly = TRUE) # save to restore later
	  par(mai = c(1, 0.5, 1, 1))
	  # Note on next call: if ref = NULL it is not really included in which
	  # so it's not requested (try tst <- c(1, 2, NULL, 4); tst[3] = 4; length(tst) = 3)
	  .plotEngine(spectra, which = c(ref, Ldone), lvls = lvls, cols = cols, ...)
	  on.exit(par(op)) # restore original values
  } # end of plot = TRUE
  
  invisible(spectra)
}
