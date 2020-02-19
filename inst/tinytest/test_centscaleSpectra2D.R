
### Unit tests for centscaleSpectra2D

# A tiny Spectra2D object for testing 
tiny <- vector("list")
tiny$F2 <- as.numeric(1:10)
tiny$F1 <- as.numeric(1:5)
tiny$data <- list( # frontal slabs of all 1's, all 2's, all 3's
	Sample_1 = matrix(6.0, nrow = 5, ncol = 10),
	Sample_2 = matrix(4.0, nrow = 5, ncol = 10),
	Sample_3 = matrix(2.0, nrow = 5, ncol = 10)
	)
tiny$names <- paste("Sample", 1:3, sep = "_")
tiny$groups <- factor(rep("A", 3))
tiny$colors <- rep("black", 3)
tiny$units <- c("ppm", "ppm", "intensity")
tiny$desc <- "Tiny data set"
class(tiny) <- "Spectra2D"
chkSpectra(tiny)

# Check that log and centering cannot be combined
expect_error(
  centscaleSpectra2D(tiny, center = TRUE, scale = "log"),
  "Cannot take log of centered data")

# Check that calling the function but with arguments that amount to doing
# nothing returns the original object
expect_equal(
  centscaleSpectra2D(tiny, center = FALSE, scale = "noscale"),
  tiny)

# Check that centering gives a correct numeric answer
tiny_c <- centscaleSpectra2D(tiny, center = TRUE, scale = "noscale")
v1 <- as.vector(tiny_c$data[[1]])
v2 <- as.vector(tiny_c$data[[2]])
v3 <- as.vector(tiny_c$data[[3]])

expect_true(isTRUE(all.equal(v1, rep(2.0, 50))))
expect_true(isTRUE(all.equal(v2, rep(0.0, 50))))
expect_true(isTRUE(all.equal(v3, rep(-2.0, 50))))
