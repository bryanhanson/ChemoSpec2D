
### Unit tests for .makeArray

# .makeArray
data(MUD1)
A <- ChemoSpec2D:::.makeArray(MUD1)
dimnames(A) <- NULL
expect_silent(for (i in 1:(dim(A)[3])) stopifnot(identical(MUD1$data[[i]], A[,,i])))

# .makeArray2 tests
A <- ChemoSpec2D:::.makeArray2(MUD1, which = 1:10)
dimnames(A) <- NULL
expect_silent(for (i in 1:(dim(A)[1])) stopifnot(identical(MUD1$data[[i]], A[i,,])))
