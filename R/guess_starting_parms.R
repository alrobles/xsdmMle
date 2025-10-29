#' get_start_parms. Generates a Latin hypercube design for the parameters
#' based on the Sobol' low-discrepancy sequence. 
#' Given a set of ranges of environmental variables create a sample
#' of parameters. 
#'
#' @param ranges A data frame with ranges to generate the parameter hypercube of parameters
#' @param numstarts The number of require samples
#'
#' @returns A data frame with colums the parameters and  numstarts rows of parameters
guess_starting_parms <- function(ranges, numstarts = 100){
  
  # infer actual start parameters, math (?) scale
  lower = ranges[, "lower"]
  names(lower) <- rownames(ranges)
  center <- ranges[, "center"]
  names(center) <- rownames(ranges)
  upper <- ranges[, "upper"]
  names(upper) <- rownames(ranges)
  
  # startparms_math_low = pomp::sobol_design(lower = lower, upper = center, nseq = numstarts/2)
  # startparms_math_up = pomp::sobol_design(lower = center, upper = upper, nseq = numstarts/2)
  # startparms_math <- rbind(startparms_math_low, startparms_math_up)
  startparms_math <- pomp::sobol_design(lower = lower, upper = upper, nseq = numstarts - 1)
  startparms_math <- rbind(startparms_math, center)
  
  return(tibble::as_tibble(startparms_math))

}
