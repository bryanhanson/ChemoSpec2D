% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import2Dspectra.R
\name{import2Dspectra}
\alias{import2Dspectra}
\title{Import 2D Spectroscopic Data}
\usage{
import2Dspectra(file, fmt, nF2, debug = 0, ...)
}
\arguments{
\item{file}{Character string giving the path to a file containing a 2D spectrum.}

\item{fmt}{Character string giving the format code to use.  Details below.}

\item{nF2}{Integer giving the number of data points in the F2 (x) dimension.
Note: If \emph{any} dimension is zero-filled you may need to study
the acquistion details to get the correct value for this argument.
This may be vendor-dependent.}

\item{debug}{Integer.  Applies to \code{fmt = "dx"} only.  See \code{\link[readJDX]{readJDX}}
for details.}

\item{...}{Arguments to be passed to \code{\link[utils]{read.table}}, or \code{readJDX}.
For \code{read.table}, \pkg{You MUST supply values for \code{sep}, \code{dec} and \code{header} consistent
with your file structure, unless they are the same as the defaults for \code{\link[utils]{read.table}}}.}
}
\value{
A list with 3 elements:
        \itemize{
          \item A matrix of the z values.  The no. of columns = \code{nF2} and the no. of rows
                follows from the size of the imported data.
          \item A vector giving the F2 (x) values.
          \item A vector giving the F1 (y) values.
        }
}
\description{
This function imports a single file (for instance, a csv) containing a 2D
spectroscopic data set.  The current version handles various types of
ASCII text files as well as a few other types.
This function is called by \code{\link{files2Spectra2DObject}}
and is exported and documented to assist in developing new format codes.
}
\section{Format Codes for Plain-Text ASCII Files}{

ASCII format codes are constructed in two parts separated by a hyphen.
Three or more columns are expected. The first part gives
the order of the columns in the file, e.g. F2F1R means the first column has the F2 values,
the second column has the F1 values and the third the real-valued intensities.  The second part of the
format code relates to the order of the rows, i.e. which column varies fastest and in what direction.
These codes are best understood in relation to how the data is stored internally in a matrix.
The internal matrix is organized exactly as the data appears on the screen, with F2 decreasing
left-to-right, and F1 increasing top-to-bottom. There are many possible formats (only those listed
are implemented, please e-mail for help creating additional combinations):
\itemize{
  \item \code{"F2F1R-F2decF1dec"} Columns in the file are F2 (x), F1 (y), real.  Both F2 and F1 are decreasing.
        Last row is first in the file.  This format is used at least some of the time by
        nmrPipe.
  \item \code{fmt = "F1F2RI-F1decF2dec2"} Columns in the file are F1 (y), F2 (x), real and imaginary
        (imaginary data will be skipped).  F1 is held at a fixed value while F2 decreases.  F1 starts high
        and decreases, so last row is first in the file. There are two sets of data in the file:
        The data after FT'ing along F2 only, and the data after FT'ing along both dimensions.  The "2"
        in the format name means we are taking the second data set.
        This format is used by JEOL when exporting to "generic ascii".  Argument \code{nF2} is ignored
        with this format as the value is sought from the corresponding \code{*.hdr} file.  Doing so
        also allows one to import files with slightly different F1 and or F2, but for this to be successful
        you will need to 1) set \code{allowSloppy = TRUE} in the call to \code{files2Spectra2DObject} and
        2) harmonize the dimensions manually after initial import.
}
}

\section{Other Format Codes}{

Here are some other format codes you can use:
\itemize{
  \item \code{"SimpleM"}.  Imports matrices composed of z values.  The F2 and F1 values
        are created from the dimension of the matrix.  After import, you will have to manually
        convert the F2 and F1 values to ppm.  You may also have to transpose the matrices, or
        perhaps invert the order of the rows or columns.  Imported via \code{read.table}.
  \item \code{"Btotxt"}.  This format imports Bruker data written to a file using the Bruker
        "totxt" command.  Tested with TopSpin 4.0.7.  This format is read via \code{readLines}
        and thus the \ldots argument does not apply.
  \item \code{"dx"}.  This format imports files written in the JCAMP-DX format, via
        package \code{\link[readJDX]{readJDX}}.
}
}

\author{
Bryan A. Hanson, DePauw University.
}
\keyword{import}
