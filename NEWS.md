
# ChemoSpec2D 0.2.19 2019-02-28

## Bug Fixes
* Both `miaLoadings` and `pfacLoadings` were incorrectly reordering their matrices.
* `inspectLvls` was not excluding any loading matrices when `loading = NULL`.

## Improvements
* `miaLoadings` and `pfacLoadings` now check to see if the requested loading has already been computed, and if so, it is not computed again. Loadings are  named `Loading_x` in the `Spectra2D` object. These functions also gain a plot argument so that plots can be suppressed if desired.  These changes were made for dealing with large data sets which can occupy a lot of memory and slow down the computation of the contours.  Using `plot = FALSE` allows one to compute the loadings and then figure out desirable contour levels before running with `plot = TRUE`.
* `pfacSpectra2D` gains an argument `setup`.  If `TRUE` and `parallel = TRUE` the parallel computational environment is automatically configured for the user.  If `FALSE` the user is responsible for setting up the environment.  This is useful if working on Azure or AWS EC2.

# ChemoSpec2D 0.2.0 2018-11-30

## New Features
* Function `plotSlice` added.

## Improvements
* Function `showScale` now opens a pdf document showing the scale used with `plotSpectra2D`.
* Improved error message from `inspectLvls`.
* Numerous documentation improvements, including vignette.
* Improved examples.
* Test and demo data set `MUD1` rebuilt.

## Bug Fixes
* Internal function .computeTicks was not detecting discontinuities in a robust manner.

## Misc.
* First release to CRAN!

# ChemoSpec2D 0.1.623 2018-11-09
## Improvements
* `plotSpectra2D` now accepts `xlim` and `ylim`.
* Ticks in `plotSpectra2D` now chosen via `pretty()` and are easier to interpret.
* `plotSpectra2D` gains an option for a grid aligned with the ticks.

## Misc.
* Consistent argument checking implemented via `ChemoSpecUtils::.chkArgs`.

# ChemoSpec2D 0.1.532 2018-10-28
## New Features
* `hcaScores` added via `ChemoSpecUtils`.

## Misc.
* Checked against devel version of `ChemoSpecUtils`.

# ChemoSpec2D 0.1.505 2018-10-20
## Misc.
* Ellipses in score plots working.

# ChemoSpec2D 0.1.471 2018-10-03
## Misc.
* Improved documentation.
* Modifications to work with `ChemoSpecUtils`.

# ChemoSpec2D 0.1.458 2018-08-07
## New Features
* Added function `plotScree`, which works with `miaSpectra2D`.
* Added `miaLoadings`.

# ChemoSpec2D 0.1.451 2018-08-30
## Improvements
* `groupNcolor` moved to non-exported functions, and re-named to `.groupNcolors`.
* `import2Dspectra` extensively reworked to fix problems and allow for easy expansion in the future.
* `files2Spectra2DObject` tweaked a bit.
* centerSpectra2D becomes `centscaleSpectra2D` and now centers and optionally scales the data.

# ChemoSpec2D 0.1.434 2018-08-20
## Misc.
* Vignette updated.

# ChemoSpec2D 0.1.431 2018-08-19
## Improvements
* Modified `plotSpectra2D` so that it will plot more than one spectrum.

# ChemoSpec2D 0.1.419 2018-08-18
## Misc.
* Added unit tests for `.findNA`.
* Setting up unit tests for `.findNA` led to code clean up (better variable names and code notes).
* Added unit tests for `centerSpectra2D`.
* Added unit tests for `chkSpectra2D`.

# ChemoSpec2D 0.1.412 2018-08-17
## Misc.
* `pfacLoadings` now returns the modified `Spectra2D` object, not the loadings matrix.
* `inspectLvls` can now access the loadings matrix, if present, in a `Spectra2D` object.

# ChemoSpec2D 0.1.407 2018-08-16
## New Features
* A simple means of drawing a separate scale/legend for the contour plots was added.

## Bug Fixes
* An error in the construction of the data cube / array in `pfacSpectra2D` was fixed.  This task is now handled by helper function `.makeArray` since it is also required by `centerSpectra2D`.
## Misc.
* `pfacLoadings` simplified, and some logical errors fixed.

# ChemoSpec2D 0.1.373 2018-08-15
## Misc.
* `MUD1` gains some negative peaks for more comprehensive graphical tests.
* `.mapColors` overhauled.
* `.plotEngine` overhauled.
* `.normAroundZero` overhauled, and renamed to `.symAroundZero`.
* Added convenience function `inspectLvls` to allow quick selection of levels.

# ChemoSpec2D 0.1.328 2018-08-12
## New Features
* Added `centerSpectra2D` function.

## Misc.
* Improved `MUD1` test data set.

# ChemoSpec2D 0.1.314 2018-08-07
## Bug Fixes
* Fixed incorrectly reversed F1 frequency removal in `removePeaks2D`.
* Fixed a problem with where the gray "no data" lines were drawn in `removePeaks2D` (part of the problem was in `.findNA`).

## Improvements
* `sumSpectra2D` now reports the number of data points in F2 and the number of slices in F1.
* `chkSpectra2D` does a better job of checking and reporting about extra data.

## Misc.
* Lots of documentation polishing.

# ChemoSpec2D 0.1.295 2018-08-06
## Misc.
* Rebuilt under latest R and updated packages.

# ChemoSpec2D 0.0.0 2017-09-06
## Misc.
* Package development begins with a port of relevant `ChemoSpec` materials.
* No NEWS entries until we have an initial framework in decent shape.