#' Standard logistic function
#'
#' Standard logistic function, also called expit.
#'
#' @param x Numeric
#'
#' @returns The expit of `x`
#' @export
#'
#' @examples
#' expit(0)
#' expit(0.5)
#' expit(-1)
expit <- function (x) 
{
  out <- exp(x) / (1 + exp(x))
  # out[x > 100] <- 1 # why needed?
  return(out)
}