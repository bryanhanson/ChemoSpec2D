[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

## What is ChemoSpec2D?

`ChemoSpec2D` is  collection of functions for working with 2D spectra.  It has been developed with NMR in mind, but other types of 2D spectroscopy may also be analyzed. `ChemoSpec2D` takes many of its cues from `ChemoSpec` and tries to create consistent graphical output and be very user friendly.  Tools to plot 2D spectra and carry out PARAFAC are the main features.

## How to install ChemoSpec2D from Github Using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "bryanhanson/ChemoSpec2D@master")
library("ChemoSpec2D")
````

`ChemoSpec2D` has a companion package containing test data called `ChemoSpec2Ddata`.  Click [here](https://github.com/bryanhanson/ChemoSpec2Ddata) for installation instructions.

### License Information

`ChemoSpec2D` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
