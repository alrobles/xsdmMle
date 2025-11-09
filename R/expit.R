#' Functions to take the expit of numerical vectors.
#' expit exp(x)/(1 + exp(x))
#'
#' @param x A numeric value
#'
#' @returns A real vector corresponding to the expits of x
#' @export
#'
#' @examples
#' expit(0)
#' expit(0.5)
#' expit(-1)
expit <- function(x) {
  out <- exp(x) / (1 + exp(x))
  out[x > 100] <- 1
  out
}
