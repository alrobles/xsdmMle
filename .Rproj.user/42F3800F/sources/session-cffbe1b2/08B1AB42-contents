#' Generalized expit function
#'
#' @param x A real number to ma
#' @param L Lower bound
#' @param U Upper bound
#' @param x0 Middle point
#'
#' @returns A rescaled valued between L and U with the middle point x0
#' @export
#'
#' @examples
#' expit_gen(3, -pi, pi, 0)
expit_gen <- function(x, L, U, x0){
  
  out <- L + (U - L)/(1 + exp(-(x - x0)))
  out[x > 100] <- U
  out[x < -100] <- L
  return(out)
}