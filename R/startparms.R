#' startparms Starting parameters for the optimization.
#' Given a set of ranges of values of environmental variables where the
#' presence of a species occurs,generates a Latin hypercube design
#' for the parameters based on the Sobol' low-discrepancy sequence.
#'
#' @param envdat The environmental array given the observed ocurrences
#' @param quant_vec A vector of quantiles. Are the bounds of the range to create
#' the hypercube
#' @param numstarts The number of samples of the hypercube
#'
#' @returns A data frame with samples for each parameter to optimize
#' @export
#'
#' @examples
#' envdat_ex_occ <- envdat_ex[, , occExample == 1]
#' startparms(envdat_ex_occ)
startparms <- function(envdat, quant_vec = c(0.1, 0.5, 0.9), numstarts = 100) {
  range_df <- get_range_df(envdat, quant_vec)
  get_start_parms(range_df, numstarts)
}
