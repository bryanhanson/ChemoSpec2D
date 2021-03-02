# ChemoSpec2D 0.4.xxxxx
## Changes in ChemoSpecUtils that affect ChemoSpec2D
* Documentation of `...` in `sumSpectra` corrected to show how to pass `tol` to `check4Gaps`.
* Option to use `Col7` a palette of 7 colorblind-friendly colors added to `files2Spectra2DObject`.

## Misc.
* Checked against `R` 4.04 RC

# ChemoSpec2D 0.4.176 2020-08-30
## Misc.
* Documentation (`pkgdown` site) is now automated via CI.
* Build & check via CI now active.

# ChemoSpec2D 0.4.156 2020-03-02
## Bugs Fixed
* Reporting of shifts from `hats_alignSpectra2D` has been corrected.  Only the reporting was foobar'ed.  The alignment worked fine.  Defaults for plots also improved.

# ChemoSpec2D 0.4.147 2020-02-18
## New Features: Importing Spectra
* Format `Btotxt` added to `import2DSpectra`, allowing import of 2D data exported by the Bruker command "totxt".
* Format `dx` added to `import2DSpectra` for use with JCAMP-DX files, via package `readJDX` which has recently learned how to import 2D NMR data sets.
* Format `F1F2RI-F1decF2dec2` added to `import2DSpectra` which handles the import of JEOL spectra exported as "generic ascii".
* `files2Spectra2DObject` gains a new argument `allowSloppy`.  This will allow one to import data sets that do not have the same dimensions.  The intent here is to deal with data sets where the number of points in each dimension is similar but not identical.  This is an experimental feature, and additional functions will be needed to handle this kind of data.  See the documentation for details.
* `files2Spectra2DObject` gains a progress bar and will now accept a path and other arguments to `list.files`, bringing it in line with `ChemoSpec::files2SpectraObject`.

## New Features: Miscellaneous
* Function `normSpectra2D` gets a new method to scale spectra on [-1 ... 1].
* Function `hats_alignSpectra2D` gains new arguments `dist_method` and `maximize` which allows the user to pass their choice of distance measure through to the objective function used to evaluate the overlap of the spectra.  This greatly improves the quality of the alignment.  See the documentation for additional details.

## New Functions
* New function `computeVolume` added to aid in normalizing spectra to particular chemical shift regions, which are volumes when the intensity is taken into account.
* Two new convenience functions, `LofL` and `LofC`, added to assist with overlaying multiple spectra in `plotSpectra2D`.  `LofL` = "List of Levels" and `LofC` = "List of Colors."

## Significant Changes
* The basic color scheme for contours was updated to use a perceptually consistent low/blue -> high/red scheme, based on the principles in the `colorspace` package.  The color-handling infrastructure was also changed to allow easy introduction of different color schemes in the future, though the user cannot yet make changes on the fly.
* `inspectLvls` simplified and arguments changed; one can now inspect just a single spectrum or a range of spectra.
* Function `calcLvls` rebuilt to be more consistent and logical.  Values will change slightly from previous values.

## Improvements
* Tick positions for `plotSpectra2D` when user specifies `xlim` and/or `ylim` is much more robust.  Function `.computeTicks` was removed as it was no longer needed.
* Tick positions for `plotSlice` similarly made more robust.
* Format options in `import2Dspectra` cleaned up (documentation and code).

## Bugs avoided
* `pfacSpectra2D` now allows control of the number of cores in use when using parallel processing.  This is to avoid multiple processes on the same shared machine each trying to use all the cores for themselves.  Per suggestion by Henrik Bengtsson on Twitter.

## Bugs fixed
* `inspectLvls` was not playing nice when argument `which` was a vector.
* Fixed a problem with `.mapColors` in which `NA` could be returned as a color, which results in an unexpected black contour.  Now removes `NA` and gives a warning that the requested levels are beyond the range of the data.

## Changes in ChemoSpecUtils that affect ChemoSpec2D
* New color and symbol schemes for the groups are now provided for use during the import process.

## Misc.
* Documentation updates and improvement.
* Added documentation for `updateGroups` which has been in `ChemoSpecUtils` for a while but effectively hidden from users of `ChemoSpec2D`.
* Cleaned up some `roxygen2` warnings.
* Unit test framework converted to `tinytest`.
* Removed `robustbase` from suggests (not needed).
* `.makeArray` gains a unit test.
* Function `.rescale` rebuilt to be more flexible.

# ChemoSpec2D 0.3.166 2019-06-09

## Bug Fixes
* An issue with the setting of x and y limits in `plotScores` was fixed.  This was a long standing bug that somehow escaped notice, dating to the early days of `ChemoSpec`. Note that `plotScores` is actually in `ChemoSpecUtils` but is called from `ChemoSpec2D`, affecting the results here.
* Scree plots for `class(mia)` were plotting the eigenvalues instead of the percent variance explained.
* The computation of the loading pseudo-spectrum for `class(mia)` was incorrect and gave a rotated version of the correct result.

## New Features
* A function to align spectra, `hats_alignSpectra2D` was added, along with a number of supporting functions.
* `conColScheme` has moved to `ChemoSpecUtils` and can now also handle `Spectra2D` objects.  It is also now more user friendly.
* New function `shiftSpectra2D` added to permit manual shifting.
* `centscaleSpectra2D` gains the ability to scale by log or log10.

## Significant Changes
* Data set `MUD1` was completely rebuilt.
* New data set `MUD2` added, for purposes of testing alignment algorithms.

## Possible Breaking Changes
* `centscaleSpectra2D` defaults have changed.

## Misc.
* Documentation `colorSymbol` was moved to package `ChemoSpecUtils`.
* Unit testing framework changed to `tinytest`.
* `normSpectra2D` now checks the input method as a valid choice.

# ChemoSpec2D 0.2.55 2019-04-30

## New Features
* New function `popSpectra2D` computes "plain old PCA" on a `Spectra2D` data set, using the IRLBA algorithm. The data is unstacked before PCA.

## Improvements
* `pfacSpectra2D` now has a `nfac` as an argument; previously the user was warned to provide it.  This is clearer, more user-friendly and more consistent with other functions.
* Small changes to several functions to work more consistently with changes in `ChemoSpecUtils` and `ChemoSpec` which introduce more PCA variants.
* `miaLoadings`, `popLoadings` (never publically released), and `pfacLoadings` were collapsed into `plotLoadings2D`.
* Improved reporting in `chkSpectra`.
* NAMESPACE cleaned up a bit.
* `files2Spectra2DObject` is now careful to remove any dimnames of the imported matrices, to avoid causing a ruckus with `chkSpectra`.

## Deprecated Functions
* `toChemoSpec` was removed as `popSpectra2D` provides a complete workflow corresponding to unstacking, computing PCA and reassembling the loadings.

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
