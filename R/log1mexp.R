#' Compute log(1 ± exp(-a)) Accurately and Efficiently
#'
#' This function computes \eqn{\log(1 - \exp(-a))} or \eqn{\log(1 + \exp(x))} in
#' a numerically stable way, avoiding catastrophic cancellation for small or
#' large values of \code{a}.
#' The implementation is adapted from the \pkg{copula} package.
#'
#' @param a \code{numeric} vector of non-negative values. NA values are allowed
#'  and preserved.
#' @param cutoff \code{numeric scalar}, positive threshold used to decide which
#' formula to apply. The default \code{log(2)} is near-optimal for numerical
#' stability, but any value in [0.5, 1] works well.
#'
#' @return A numeric vector of the same length as \code{a}, where each element
#' is:
#' \itemize{
#'   \item \eqn{\log(1 - \exp(-a))} for positive \code{a}, computed accurately.
#'   \item Preserves \code{NA} values from input.
#' }
#'
#' @details
#' Direct computation of \eqn{\log(1 - \exp(-a))} can suffer from floating-point
#'  errors when \code{a} is small or large. This function switches between:
#' \itemize{
#'   \item \code{log(-expm1(-a))} when \code{a <= cutoff}, for better precision
#'    in small \code{a}.
#'   \item \code{log1p(-exp(-a))} when \code{a > cutoff}, for better precision
#'    in large \code{a}.
#' }
#'
#' @note
#' If any \code{a < 0}, a warning is issued since the formula assumes
#' \code{a >= 0}.
#'
#' @examples
#' # Compare different approaches for a range of values
#' a <- 2^seq(-58, 10, length = 256)
#' f_expr <- expression(
#'   log(1 - exp(-a)),
#'   log(-expm1(-a)),
#'   log1p(-exp(-a)),
#'   log1mexp(a)
#' )
#' names(f_expr) <- c("Direct", "expm1", "log1p", "Stable")
#' fa <- do.call(cbind, as.list(f_expr))
#' head(fa) # expm1() works well for small a
#' tail(fa) # log1p() works well for large a
#'
#' @seealso \code{\link{log1p}}, \code{\link{expm1}}
#' @export
log1mexp <- function(a, cutoff = log(2)) {
  if (has_na <- any(ina <- is.na(a))) {
    y <- a
    a <- a[ok <- !ina]
  }
  if (any(a < 0)) {
    warning("'a' >= 0 needed")
  }
  tst <- a <= cutoff
  r <- a
  r[tst] <- log(-expm1(-a[tst]))
  r[!tst] <- log1p(-exp(-a[!tst]))
  if (has_na) {
    y[ok] <- r
    y
  } else {
    r
  }
}
