#' Compute f(a) = log(1 +/- exp(-a)) Numerically Optimally.
#' Original function by Martin Mächler (2012) in copula R pacakge
#' @param x Numeric vector
#' @param c0 Cutoffs for log1pexp
#' @param c1 Cutoffs for log1pexp
#' @param c2 Cutoffs for log1pexp
#'
#' @returns f(a) == log(1 - exp(-a)) == log1p(-exp(-a)) == log(-expm1(-a))
#' or g(x) == log(1 + exp(x)) == log1p(exp(x))
#' computed accurately and quickly
#' @export
#'
#' @examples
#' x <- seq(700, 720, by=2)
#' cbind(x, log1p(exp(x)), log1pexp(x))
log1pexp <- function(x, c0 = -37, c1 = 18, c2 = 33.3) {
  if (has_na <- any(ina <- is.na(x))) {
    y <- x
    x <- x[ok <- !ina]
  }
  r <- exp(x)
  if (any(i <- c0 < x & (i1 <- x <= c1)))
    r[i] <- log1p(r[i])
  if (any(i <- !i1 & (i2 <- x <= c2)))
    r[i] <- x[i] + 1 / r[i]
  if (any(i3 <- !i2))
    r[i3] <- x[i3]
  if (has_na) {
    y[ok] <- r
    y
  } else {
    r
  }
}
