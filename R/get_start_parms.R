#' get_start_parms. Generates a Latin hypercube design for the parameters
#' based on the Sobol' low-discrepancy sequence.
#' Given a set of ranges of environmental variables create a sample
#' of parameters.
#'
#' @param ranges A data frame with ranges to generate the parameter hypercube
#' of parameters
#' @param numstarts The number of require samples
#'
#' @returns A data frame with columns the parameters and rows the number of sets
#' of starting parameters
get_start_parms <- function(ranges, numstarts = 100) {
  # Check parameters
  checkmate::assert_data_frame(ranges, any.missing = FALSE, ncols = 3)
  checkmate::assert_number(numstarts)
  checkmate::assert_names(names(ranges),
                          must.include = c("lower", "center", "upper"))
  # Set limits on the parameters
  lower <- ranges[, 1]
  names(lower) <- rownames(ranges)
  center <- ranges[, 2]
  names(center) <- rownames(ranges)
  upper <- ranges[, 3]
  names(upper) <- rownames(ranges)

  # Get the actual start parameters, math scale
  startparms_math <- pomp::sobol_design(
    lower = lower,
    upper = upper,
    nseq = numstarts - 1
  )
  startparms_math <- rbind(startparms_math, center)
  tibble::as_tibble(startparms_math)
}
