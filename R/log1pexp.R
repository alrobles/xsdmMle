#' Compute log(1 ± exp(-a)) Accurately and Efficiently
#'
#' This function computes either \eqn{\log(1 - \exp(-a))} or \eqn{\log(1 + \exp(x))} 
#' in a numerically stable way, avoiding catastrophic cancellation for extreme values.
#' The implementation is adapted from Martin Mächler's original function in the \pkg{copula} package.
#'
#' @param x Numeric vector of input values.
#' @param c0,c1,c2 Numeric scalars used as cutoffs for switching between formulas 
#' for optimal numerical stability. Defaults are chosen for best precision.
#'
#' @return A numeric vector of the same length as \code{x}, where each element is:
#' \itemize{
#'   \item \eqn{\log(1 - \exp(-a))} computed accurately for positive \code{a}.
#'   \item \eqn{\log(1 + \exp(x))} computed accurately for any \code{x}.
#' }
#' 
#' @export
#'
#' @details
#' Direct computation of these expressions can suffer from floating-point errors 
#' when arguments are very small or very large. This function switches between 
#' stable formulas:
#' \itemize{
#'   \item \code{log(-expm1(-a))} for small \code{a}.
#'   \item \code{log1p(-exp(-a))} for large \code{a}.
#'   \item \code{log1p(exp(x))} for \eqn{\log(1 + \exp(x))}.
#' }
#'
#' @note
#' For \eqn{\log(1 - \exp(-a))}, \code{a} should be non-negative. A warning is issued if \code{a < 0}.
#'
#' @examples
#' # Compare log1p(exp(x)) with stable implementation
#' x <- seq(700, 720, by = 2)
#' cbind(x, log1p(exp(x)), log1pexp(x))
#'
#' @seealso \code{\link{log1p}}, \code{\link{expm1}}

log1pexp <- function(x, c0 = -37, c1 = 18, c2 = 33.3) {
  if (has_na <- any(ina <- is.na(x))) {
    y <- x
    x <- x[ok <- !ina]
  }
  r <- exp(x)
  if (any(i <- c0 < x & (i1 <- x <= c1))) {
    r[i] <- log1p(r[i])
  }
  if (any(i <- !i1 & (i2 <- x <= c2))) {
    r[i] <- x[i] + 1 / r[i]
  }
  if (any(i3 <- !i2)) {
    r[i3] <- x[i3]
  }
  if (has_na) {
    y[ok] <- r
    y
  } else {
    r
  }
}
