#' Logistic function
#'
#' Standard logistic function, also called expit.
#'
#' Standard logistic function, also called expit.
#' @param x Numeric
#' @param L Numeric, lower bound
#' @param U Numeric, upper bound
#' @param x0 Numeric, middle point
#'
#' @returns A rescaled valued between L and U with the middle point at x0
#' @export
#'
#' @examples
#' expit_gen(3, -pi, pi, 0)
expit_gen <- function(x, L, U, x0){
  
  out <- L + (U - L)/(1 + exp(-(x - x0)))
  # why the next two lines needed?
  # out[x > 100] <- U
  # out[x < -100] <- L

  return(out)
}