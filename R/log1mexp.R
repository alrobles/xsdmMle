#' Compute f(a) = log(1 +/- exp(-a)) Numerically Optimally.
#' Original code in copula R package.
#'
#' @param a numeric vector of positive values
#' @param cutoff positive number; log(2) is "optimal", but the exact value is
#' unimportant, and anything in [0.5, 1] is fine.
#'
#' @returns f(a) == log(1 - exp(-a)) == log1p(-exp(-a)) == log(-expm1(-a))
#' or g(x) == log(1 + exp(x)) == log1p(exp(x)) computed accurately and quickly
#' @export
#'
#' @examples
#' a <- 2^seq(-58,10, length = 256)
#' f_expr <- expression(
#' log(1 - exp(-a)),
#' log(-expm1(-a)),
#' log1p(-exp(-a)),
#' log1mexp(a))
#' names(f_expr) <- c("DEF", "expm1", "log1p", "F")
#' str(fa <- do.call(cbind, as.list(f_expr)))
#' head(fa) # expm1() works here
#' tail(fa) # log1p() works here
log1mexp <- function(a, cutoff = log(2)) {
  if (has_na <- any(ina <- is.na(a))) {
    y <- a
    a <- a[ok <- !ina]
  }
  if (any(a < 0))
    warning("'a' >= 0 needed")
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
