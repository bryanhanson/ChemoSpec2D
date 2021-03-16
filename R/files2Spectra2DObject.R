#'
#' Import Data into a Spectra2D Object
#'
#' This function imports data into a \code{\link{Spectra2D}} object.  It primarily uses
#' \code{\link[utils]{read.table}} to read files so it is
#' very flexible in regard to file formatting.  \pkg{Be sure to see the \ldots
#' argument below for important details you need to provide.}
#'
#' \code{files2Spectra2DObject} acts on all files in the current working
#' directory with the specified \code{fileExt} so there should be no
#' extraneous files with that extension in the directory.
#'
#' @param gr.crit Group Criteria.  A vector of character strings which will be
#'        searched for among the file/sample names in order to assign an individual
#'        spectrum to group membership. This is done using grep, so characters
#'        like "." (period/dot) do not have their literal meaning (see below).
#'        Warnings are issued if there are file/sample
#'        names that don't match entries in \code{gr.crit} or there are entries in
#'        \code{gr.crit} that don't match any file names.
#'
#' @param gr.cols Group Colors.  See \code{\link{colorSymbol}} for some options. One of the following:
#'   \itemize{
#'     \item Legacy behavior and the default: The word \code{"auto"}, in which case up to 8 colors will
#'           be automatically assigned from package \code{RColorBrewer Set1}.
#'     \item \code{"Col7"}. A unique set of up to 7 colorblind-friendly colors is used.
#'     \item \code{"Col8"}. A unique set of up to 8 colors is used.
#'     \item \code{"Col12"}. A mostly paired set of up to 12 colors is used.
#'     \item A vector of acceptable color designations with the same length as \code{gr.crit}.
#'   }
#'       Colors will be assigned one for one, so the first element of
#'       \code{gr.crit} is assigned the first element of \code{gr.col} and so forth.  For \code{Col12}
#'       you should pay careful attention to the order of \code{gr.crit} in order to match up colors.
#'       See \code{\link[ChemoSpecUtils]{colorSymbol}} for further details.
#'
#' @param x.unit A character string giving the units for the F2 dimension
#'        (frequency or wavelength corresponding to the x dimension).
#'
#' @param y.unit A character string giving the units for the F1 dimension
#'        (frequency or wavelength corresponding to the y dimension).
#'
#' @param z.unit A character string giving the units of the z-axis (some sort
#'        of intensity).
#'
#' @param descrip A character string describing the data set.
#'
#' @param fmt A character string giving the format of the data. Consult
#'        \code{\link{import2Dspectra}} for options.
#'        If \code{fileExt} is one of \code{dx, DX, jdx or JDX}, \code{fmt} will automatically
#'        be set to \code{"dx"} and package \code{readJDX} will be used for the import.  In this case
#'        check the values of F2 and F1 carefully.  The values are taken from the file,
#'        for some files/vendors the values might be in Hz rather than ppm.
#'
#' @param nF2 Integer giving the number of data points in the F2 (x) dimension.
#'        Note: If \emph{any} dimension is zero-filled you may need to study
#'        the acquistion details to get the correct value for this argument.
#'        This may be vendor-dependent.
#'
#' @param fileExt A character string giving the extension of the files to be
#'        processed. \code{regex} strings can be used.  For instance, the default
#'        finds files with either \code{".csv"} or \code{".CSV"} as the extension.
#'        Matching is done via a grep process, which is greedy.
#'        If \code{fileExt} is one of \code{dx, DX, jdx or JDX}, \code{fmt} will automatically be
#'        set to \code{"dx"} and package \code{readJDX} will be used for the import.
#'
#' @param out.file A file name.  The completed object of S3 class \code{\link{Spectra2D}} will be written
#'        to this file.
#'
#' @param debug Integer.  Set to 1 or \code{TRUE} for basic reporting when there are problems.
#'        If importing JCAMP-DX files, values greater than 1 give additional and potentially
#'        huge output.  Once you know which file is the problem, you may wish to troubleshoot
#'        directly using package \code{readJDX}.
#'
#' @param chk Logical. Should the \code{Spectra} object be checked for integrity?  If you are having
#'        trouble importing your data, set this to \code{FALSE} and do \code{str(your object)} to investigate.
#'
#' @param allowSloppy Logical. \pkg{Experimental Feature} If \code{TRUE}, disable checking of the data set, and
#'        return all pieces of the raw import from \code{import2Dspectra} in the \code{spectra$data} object.
#'        The resulting object currently cannot be used by any other functions in this package!  The
#'        intent is allow importing of spectra that differ slightly in the number of points in each dimension.
#'        With this option one can use \code{str} on the resulting object to inspect the differences.
#'        Future functions will allow one to clean up the data.
#'
#' @param ...  Arguments to be passed to \code{\link[utils]{read.table}},
#'        \code{\link{list.files}} or \code{readJDX}; see the "Advanced Tricks" section.
#'        For \code{read.table}, \pkg{You MUST supply values for \code{sep}, \code{dec} and \code{header} consistent
#'        with your file structure, unless they are the same as the defaults for \code{\link[utils]{read.table}}}.
#'
#' @return One of these objects:
#'         \itemize{
#'           \item If \code{allowSloppy = FALSE}, the default, an object of class \code{\link{Spectra2D}}.
#'           \item If \code{allowSloppy = TRUE}, an object of undocumented class \code{SloppySpectra2D}.
#'                 These objects are \pkg{experimental} and are not checked by \code{chkSpectra}.
#'                 For these objects \code{spectra$F1} and \code{spectra$F2} are \code{NA}, and each
#'                 \code{spectra$data} entry is a list with elements F1, F2 and M, which is the matrix
#'                 of imported data (basically, the object returned by \code{import2Dspectra}).
#'           \item In each case,
#'                 an \emph{unnamed} object of S3 class \code{\link{Spectra2D}} or \code{SloppySpectra2D}
#'                 is also written to \code{out.file}.
#'                 To read it back into the workspace, use \code{new.name <- loadObject(out.file)}
#'                 (\code{loadObject} is package \pkg{R.utils}).
#'         }
#'
#' @section gr.crit and Sample Name Gotchas:
#'
#' The matching of \code{gr.crit} against the sample file names is done one at
#' a time, in order, using grep.  While powerful, this has the potential to lead
#' to some "gotchas" in certain cases, noted below.
#'
#' Your file system may allow file/sample names which \code{R} will not like, and will
#' cause confusing behavior.  File/sample names become variables in \code{ChemoSpec}, and \code{R}
#' does not like things like "-" (minus sign or hyphen) in file/sample names.  A hyphen
#' is converted to a period (".") if found, which is fine for a variable name.
#' However, a period in \code{gr.crit} is interpreted from the grep point of view,
#' namely a period matches any single character.  At this point, things may behave
#' very differently than one might hope.  See \code{\link{make.names}} for allowed
#' characters in \code{R} variables and make sure your file/sample names comply.
#'
#' The entries in \code{gr.crit} must be
#' mutually exclusive.  For example, if you have files with names like
#' "Control_1" and "Sample_1" and use \code{gr.crit = c("Control", "Sample")}
#' groups will be assigned as you would expect.  But, if you have file names
#' like "Control_1_Shade" and "Sample_1_Sun" you can't use \code{gr.crit =
#' c("Control", "Sample", "Sun", "Shade")} because each criteria is grepped in
#' order, and the "Sun/Shade" phrases, being last, will form the basis for your
#' groups.  Because this is a grep process, you can get around this by using
#' regular expressions in your \code{gr.crit} argument to specify the desired
#' groups in a mutually exclusive manner.  In this second example, you could
#' use \code{gr.crit = c("Control(.*)Sun"}, \code{"Control(.*)Shade"}, \code{"Sample(.*)Sun"},
#' \code{"Sample(.*)Shade")} to have your groups assigned based upon both phrases in
#' the file names.
#'
#' To summarize, \code{gr.crit} is used as a grep pattern, and the file/sample names
#' are the target.  Make sure your file/sample names comply with \code{\link{make.names}}.
#'
#' Finally, samples whose names are not matched using \code{gr.crit} are still
#' incorporated into the \code{\link{Spectra2D}} object, but they are not
#' assigned a group. Therefore they don't plot, but they do take up space in a
#' plot!  A warning is issued in these cases, since one wouldn't normally want
#' a spectrum to be orphaned this way.
#'
#' All these problems can generally be identified by running \code{\link[ChemoSpecUtils]{sumSpectra}}
#' once the data is imported.
#'
#' @section Advanced Tricks:
#'
#' The ... argument can be used to pass any argument to \code{read.table} or \code{list.files}.
#' This includes the possibility of passing arguments that will cause trouble later, for instance
#' \code{na.strings} in \code{read.table}.  While one might successfully read in data with \code{NA},
#' it will eventually cause problems.  The intent of this feature is to allow one to recurse
#' a directory tree containing the data, and/or to specify a starting point other than the current
#' working directory.  So for instance if the current working directory is not the directory containing
#' the data files, you can use \code{path = "my_path"} to point to the desired top-level
#' directory, and \code{recursive = TRUE} to work your way through a set of subdirectories.  In addition,
#' if you are reading in JCAMP-DX files, you can pass arguments to \code{readJDX} via ..., e.g. \code{SOFC = FALSE}.
#' Finally, while argument \code{fileExt} appears to be a file extension (from its
#' name and the description elsewhere), it's actually just a grep pattern that you can apply
#' to any part of the file name if you know how to construct the proper pattern.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords import
#'
#' @export files2Spectra2DObject
#'
#' @importFrom utils read.table setTxtProgressBar txtProgressBar
#' @importFrom tools file_path_sans_ext
#' @importFrom ChemoSpecUtils .groupNcolor
#'

files2Spectra2DObject <- function(gr.crit = NULL, gr.cols = "auto",
                                  fmt = NULL, nF2 = NULL,
                                  x.unit = "no frequency unit provided",
                                  y.unit = "no frequency unit provided",
                                  z.unit = "no intensity unit provided",
                                  descrip = "no description provided",
                                  fileExt = "\\.(csv|CSV)$",
                                  out.file = "mydata", debug = 0, chk = TRUE,
                                  allowSloppy = FALSE, ...) {
  if (!requireNamespace("R.utils", quietly = TRUE)) {
    stop("You need to install package R.utils to use this function")
  }

  if (is.null(gr.crit)) stop("No group criteria provided to encode data")

  if (grepl("(dx|DX|jdx|JDX)", fileExt)) {
    fmt <- "dx"
    if (!requireNamespace("readJDX", quietly = TRUE)) {
      stop("You need to install package readJDX to import JCAMP-DX files")
    }
  }

  if (is.null(fmt)) stop("You must provide fmt")

  if (is.null(nF2)) {
    if (!fmt %in% c("Btotxt", "dx", "F1F2RI-F1decF2dec2")) stop("You must provide nF2 for this fmt")
  }

  out <- tryCatch(
    {

      # First set up some common stuff

      # Clean up args found in ... for further use
      argsLF <- as.list(match.call())[-1]
      argsLF <- .cleanArgs2D(argsLF, "list.files")
      argsLF <- c(argsLF, list(pattern = fileExt, full.names = TRUE))

      files <- do.call(list.files, argsLF)
      if (length(files) == 0L) stop("No files found. Is the extension wrong, or are we in the wrong directory?")
      files.noext <- tools::file_path_sans_ext(basename(files))

      ns <- length(files)

      spectra <- list()
      spectra$F2 <- NA_real_
      spectra$F1 <- NA_real_
      spectra$data <- vector("list", ns)
      spectra$names <- files.noext
      names(spectra$data) <- files.noext
      spectra$groups <- NA_character_
      spectra$units <- c(x.unit, y.unit, z.unit)
      spectra$desc <- descrip
      class(spectra) <- "Spectra2D"

      # Loop over all files

      if (debug > 0L) message("\nfiles2Spectra2DObject will now import your files")

      if (!debug) {
        # Code for progress bar contributed by Reinhard Kerschner
        env <- environment() # NEW set environment for progress bar
        pb_Max <- length(files)
        counter <- 0
        message("\nReading ", pb_Max, " files...\n")
        pb <- txtProgressBar(min = 0, max = pb_Max, style = 3)
      }

      for (i in 1:ns) {
        if (debug > 0L) cat("Importing file: ", files[i], "\n")
        tmp <- import2Dspectra(files[i], fmt = fmt, nF2 = nF2, debug = debug, ...)

        if (!allowSloppy) {
          spectra$data[[i]] <- tmp[["M"]]
          dimnames(spectra$data[[i]]) <- NULL # clean up to plain matrix
          if (i == 1L) {
            spectra$F2 <- tmp[["F2"]]
            spectra$F1 <- tmp[["F1"]]
          }
        }

        if (allowSloppy) {
          spectra$data[[i]] <- tmp
          dimnames(spectra$data[[i]][["M"]]) <- NULL # clean up to plain matrix
          chk <- FALSE
        }

        if (!debug) {
          curVal <- get("counter", envir = env)
          assign("counter", curVal + 1, envir = env)
          setTxtProgressBar(get("pb", envir = env), curVal + 1)
        }
      }

      # Assign groups & colors

      if (!debug) {
        close(pb)
        message("\nAssigning ", pb_Max, " spectra to ", length(gr.crit), " groups...\n")
      }

      spectra <- .groupNcolor(spectra, gr.crit, gr.cols, mode = "2D")
      if (allowSloppy) {
        class(spectra) <- "SloppySpectra2D"
        message("You have set allowSloppy = TRUE.  Use str(your_object) to inspect your creation!")
      }

      # Wrap up

      if (chk) chkSpectra(spectra)

      datafile <- paste(out.file, ".RData", sep = "")
      R.utils::saveObject(spectra, file = datafile)
      return(spectra)
    },
    error = function(cond) {
      errmess <- "There was a problem importing your files!\n\nAre you importing csv or similar files? Did you get a message such as 'undefined columns selected'? You probably need to specify sep, header and dec values. Please read ?files2Spectra2DObject for details.\n\nFor any trouble importing files set debug > 0.\n"
      message("\nError message from R: ", cond$message, "\n")
      message(errmess)
      return(NA)
    }
  ) # end of tryCatch

  return(out)
}
