#' get_start_parms. Generates a Latin hypercube design for the parameters
#' based on the Sobol' low-discrepancy sequence. 
#' Given a set of ranges of environmental variables create a sample
#' of parameters. 
#'
#' @param ranges A data frame with ranges to generate the parameter hypercube of parameters
#' @param numstarts The number of require samples
#'
#' @returns A data frame with colums the parameters and  numstarts rows of parameters
get_start_parms <- function(ranges, numstarts = 100){
  
  #now get the actual start parameters, math scale
  lower = ranges[ ,1]
  names(lower) = rownames(ranges)
  
  center = ranges[ ,2]
  names(center) = rownames(ranges)
  upper = ranges[ ,3]
  names(upper) = rownames(ranges)
  
  # startparms_math_low = pomp::sobol_design(lower = lower, upper = center, nseq = numstarts/2)
  # startparms_math_up = pomp::sobol_design(lower = center, upper = upper, nseq = numstarts/2)
  #startparms_math <- rbind(startparms_math_low, startparms_math_up)
  startparms_math <- pomp::sobol_design(lower = lower, upper = upper, nseq = numstarts - 1)
  startparms_math <- rbind(startparms_math, center)
  startparms_math |> tibble::as_tibble()
}