#' Get the number of parameters of the model given the number
#' of environmental time serires
#'
#' @param p An integer with the number of environmental variables
#'
#' @returns An integer with the number of parameters
#'
#' @export
#' @examples
#' numparms(2)
numparms <- function(p) {
  # ctil, plus the O params, plus the mu, sigL and sigR params, plus pd
  1 + (p^2 - p) / 2 + 3 * p + 1
}
