#' Number of parameters
#'
#' The number of parameters of the xSDM model.
#'
#' The number of parameters is calculated based on the number of 
#'  environmental predictors used.
#' @param p An integer with the number of environmental variables
#'
#' @returns An integer with the number of parameters
#'
numparms <- function(p){
  # orthogonal matrix 
  # mu, sigL and sigR params
  # ctil
  # pd
  1 + (p^2 - p)/2 + 3 * p + 1
}
