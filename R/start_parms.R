#' startparms Starting parameters for the optimization.
#' Given a set of ranges of values of environmental variables where the
#' presence of a species occurs,generates a Latin hypercube design
#' for the parameters based on the Sobol' low-discrepancy sequence.
#'
#' @param env_dat The environmental array given the observed occurrences
#' @param quant_vec A vector of quantiles. Are the bounds of the range to create
#' the hypercube
#' @param num_starts The number of samples of the hypercube
#'
#' @returns A data frame with samples for each parameter to optimize
#' @export
#'
#' @examples
#' envdat_ex_occ <- envdat_ex[, , occExample == 1]
#' start_parms(envdat_ex_occ)
start_parms <- function(env_dat,
                        quant_vec = c(0.1, 0.5, 0.9),
                        num_starts = 100) {
  checkmate::assert_array(env_dat, min.d = 1, null.ok = FALSE)
  checkmate::assert_vector(quant_vec, len = 3, strict = TRUE, null.ok = FALSE)
  checkmate::assert_number(num_starts)

  range_df <- get_range_df(env_dat, quant_vec)
  get_start_parms(range_df, num_starts)
}
