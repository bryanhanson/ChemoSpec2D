
### Unit tests for .makeArray

# .makeArray
data(MUD1)
A <- .makeArray(MUD1)
str(A)
dimnames(A) <- NULL
expect_silent(for (i in 1:(dim(A)[3])) stopifnot(identical(MUD1$data[[i]], A[,,i])))

# .makeArray2 tests
A <- .makeArray2(MUD1, which = 1:10)
dimnames(A) <- NULL
expect_silent(for (i in 1:(dim(A)[1])) stopifnot(identical(MUD1$data[[i]], A[i,,])))
