<img src="man/figures/Banner.png"/>

<!-- Each image below is embedded in an empty link which gives space around each badge -->


[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)]() [![Build & Check](https://github.com/bryanhanson/ChemoSpec2D/workflows/Build-Check/badge.svg)]() [![Docs Current](https://github.com/bryanhanson/ChemoSpec2D/workflows/Update-Docs/badge.svg)]()

[![CRAN status](https://www.r-pkg.org/badges/version-last-release/ChemoSpec2D)]() [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/ChemoSpec2D)]() [![Downloads](https://cranlogs.r-pkg.org/badges/ChemoSpec2D)]() [![status](https://tinyverse.netlify.com/badge/ChemoSpec2D)]()

## What is ChemoSpec2D?

`ChemoSpec2D` is a collection of functions for exploratory chemometrics of 2D spectroscopic data sets such as COSY (correlated spectroscopy) and HSQC (heteronuclear single quantum coherence) 2D NMR (nuclear magnetic resonance) spectra. `ChemoSpec2D` deploys methods aimed primarily at classification of samples and the identification of spectral features which are important in distinguishing samples from each other. Each 2D spectrum (a matrix) is treated as the unit of observation, and thus the physical sample in the spectrometer corresponds to the  sample from a statistical perspective.  In addition to chemometric tools, a few tools are provided for plotting 2D spectra, but these are not intended to replace the functionality typically available on the spectrometer. `ChemoSpec2D` takes many of its cues from `ChemoSpec` and tries to create consistent graphical output and to be very user friendly.

### Installing ChemoSpec2D from CRAN:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("ChemoSpec2D")
library("ChemoSpec2D")
````

### Installing ChemoSpec2D from Github:

```r
chooseCRANmirror() # choose a CRAN mirror
install.packages("remotes")
library("remotes")
install_github(repo = "bryanhanson/ChemoSpec2D@master")
library("ChemoSpec2D")
```

### License Information

`ChemoSpec2D` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
