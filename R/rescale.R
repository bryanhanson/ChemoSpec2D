#'
#' Universal/Flexible Rescaling Function
#'
#' The last 2 arguments are subject to lazy evaluation if not provided.  In this case, the old extremes
#' are stretched to the new extremes.  If \code{old.min != min(x)} then the new values
#' are interpolated onto a new scale.  Similarly for \code{old.max}.
#'
#' @param x Numeric.  The vector to be rescaled.
#' @param new.min Numeric.  The minimum value in the new scale.
#' @param new.max Numeric.  The maximum value in the new scale.
#' @param old.min Numeric.  The minimum value in the old scale.
#' @param old.max Numeric.  The maximum value in the old scale.
#' @return A numeric vector with the rescaled values.
#'
#' @noRd
#'
#' @examples
#' # The following code produces a graphic illustrating the use of this function.
#'
#' x1 <- 1:10
#' np <- length(x1)
#' y1 <- rep(1, np)
#'
#' x2 <- .rescale(x1, -10, 15)
#' y2 <- rep(0, np)
#'
#' x3 <- .rescale(x1, 15, 19)
#' y3 <- rep(2, np)
#'
#' x4 <- .rescale(x1, -15, -20, -10, 10)
#' y4 <- rep(3, np)
#'
#' plot(x1, y1,
#'   xlim = c(-20, 20), ylim = c(0, 5), yaxt = "n", pch = 20,
#'   xlab = "", ylab = "", main = "Function .rescale Behavior"
#' )
#' text(15, 1, labels = "original")
#' grid(NULL, NA)
#' text(-5, 1.5, labels = "old.min = min(x)\nold.max = max(x)")
#' rect(-10, 1.3, 0, 1.7)
#' text(-10, 3.5, labels = "old.min = min(orig scale)\nold.max = max(orig scale)")
#' rect(-17.5, 3.30, -2.5, 3.7)
#'
#' points(x2, y2, col = "black", pch = 20)
#' for (i in 1:np) lines(x = c(x1[i], x2[i]), y = c(y1[i], y2[i]), type = "c")
#' text(-15, 0, labels = "expanded", col = "black")
#'
#' points(x3, y3, col = "black", pch = 20)
#' for (i in 1:np) lines(x = c(x1[i], x3[i]), y = c(y1[i], y3[i]), type = "c")
#' text(10, 2, labels = "shrunken", col = "black")
#'
#' segments(-10, 4, 20, 4)
#' points(x1, rep(4, np), pch = 20)
#' text(-18, 4, labels = "original\nscale", col = "black")
#'
#' segments(-5, 3, 10, 3)
#' new.x <- .rescale(x1, -5, 10, -10, 20)
#' points(new.x, rep(3, np), pch = 20)
#' for (i in 1:np) lines(x = c(x1[i], new.x[i]), y = c(4, 3), type = "c")
#' text(-18, 3, labels = "shorter\nscale", col = "black")
#'
#' segments(-15, 5, 20, 5)
#' new.x <- .rescale(x1, -15, 20, -10, 20)
#' points(new.x, rep(5, np), pch = 20)
#' for (i in 1:np) lines(x = c(x1[i], new.x[i]), y = c(4, 5), type = "c")
#' text(-18, 5, labels = "longer\nscale", col = "black")
.rescale <- function(x, new.min, new.max, old.min = min(x), old.max = max(x)) {
  new.x <- new.min + (new.max - new.min) * (x - old.min) / (old.max - old.min)
  new.x
}
