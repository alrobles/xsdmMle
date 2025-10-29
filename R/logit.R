#' Logit function.
#'
#' Standard logit function.
#'
#' Inverse of the expit function
#' @param x A numeric value to scale from 0 to 1 to -inf to inf
#'
#' @returns A numeric 
#' @export
#'
#' @examples
#' logit(0.5)
logit <- function(x)  {
  edges <- (x < 0) | (x > 1)
  out <- numeric(length(x))
  out[edges] <- NaN
  out[!edges] <- log(x[!edges]/(1 - x[!edges]))
  dim(out) <- dim(x)
  return(out)
}
