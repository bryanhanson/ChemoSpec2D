
### Unit tests for hats_alignSpectra2D
# This example is wrapped in dontrun in the Rd for hats_alignSpectra2D
# It takes about 40 seconds, so run only at home

# If running this test interactively:
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "TRUE") to permit additional tests to run
#   From R, run Sys.setenv("ESTOY_EN_CASA" = "") to run exactly as at CRAN

# If running this test via a shell script like a makefile, set the variable in the shell first:
#   setenv ESTOY_EN_CASA TRUE # csh
#   unsetenv ESTOY_EN_CASA # csh
#   export ESTOY_EN_CASA=TRUE # bash
#   export -n ESTOY_EN_CASA # bash
#   or set in the makefile

if (identical(Sys.getenv("ESTOY_EN_CASA"), "TRUE")) {
  data(MUD2)
  expect_silent(MUD2a <- hats_alignSpectra2D(MUD2, method = "MBO", dist_method = "euclidean", debug = 0, plot = FALSE))
}

