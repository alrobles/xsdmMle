#' Get range data frame. Creates a unfilled data frame
#' to store the parameter ranges given a number of environmental variables
#' @param envdat An environmental array with dimenssions given by 
#' environmental variables, time serires steps and locations.
#' Should provide an environmental array where the detections are possitive
#' @param quant_vec A vector of quantiles to limit the range of the
#' parameters given the occurrence in the environmental time serires.
#' Default is set to c(0.1, 0.5, 0.9) to center in the median value
#' @returns A data.frame with NA three columns, lower bound, center and
#' upper bound of the range of the parameters.
get_range_df <- function(envdat, quant_vec = c(0.1, 0.5, 0.9) ){
  p <- dim(envdat)[1]
  
  ranges <- data.frame( lower  = NA*numeric(numparms(p)),
                        center = NA*numeric(numparms(p)),
                        upper  = NA*numeric(numparms(p)) )
  
  q <- (p^2-p)/2
  
  O_inds = 1:(q) 
  mu_inds = (1 + q):(q + p)
  sigLtil_inds = (1 + q + p ):(q + 2*p)
  sigRtil_inds = (1 + q + 2*p ):(q + 3*p)
  pd_inds = (1 + q + 3*p):(numparms(p) - 1) 
  ctil_inds = numparms(p)
  
  rownames(ranges)[O_inds] = paste0("O",1:q)
  rownames(ranges)[mu_inds] = paste0("mu",1:p)
  rownames(ranges)[sigLtil_inds] = paste0("sigLtil", 1:p)
  rownames(ranges)[sigRtil_inds] = paste0("sigRtil", 1:p)
  rownames(ranges)[pd_inds] = "pd"
  rownames(ranges)[ctil_inds] = "ctil"
  
  #get reasonable ranges of mu, sigL and sigR values
  for (counter in 1:p)
  {
    h = as.numeric(envdat[counter, , ])
    
    #get the mu range
    ranges[mu_inds[counter], ] = unname(stats::quantile(h, probs = quant_vec))
    
    #get mu center
    mu_center = ranges[mu_inds[counter], 2]
    
    #get the sigL range
    h2 = sqrt(mean( (h[ h < mu_center] - mu_center)^2) )
    ranges[sigLtil_inds[counter],] = log(c(h2/2, h2, 2* h2)) #the log is because we want these on the math scale
    
    # h2_l <- sqrt( (h[ h < mu_center] - mu_center)^2) 
    # ranges[sigLtil_inds[counter], ] <- stats::quantile(h2_l, probs = quant_vec) |> log() #the log is because we want these on the math scale
    # 
    
    #get the sigR range
    h2 = sqrt(mean((h[ h > mu_center] - mu_center)^2))
    ranges[sigRtil_inds[counter], ] = log(c(h2/2, h2, 2*h2)) #the log is because we want these on the math scale
    #h2_r = sqrt( (h[ h > mu_center] - mu_center)^2)
    # ranges[sigRtil_inds[counter], ] <- stats::quantile(h2_r, probs = quant_vec) |> log() #the log is because we want these on the math scale
    # 
    
  }
  
  # O parameters. This does not matter too much because of the main-to-one
  # nature of the exponential map.
  
  
  ranges[O_inds, 1] = -pi
  ranges[O_inds, 2] = 0
  ranges[O_inds, 3] = pi
  
  #pd parameters
  ranges[pd_inds,] = logit(quant_vec)
  
  ## ctil range. 
  
  # 1. Evaluate lolik in  central values of all the other parameters
  # 2. Pick the central ctil to be the opposite of the median of those values.
  
  #  For those central parameters
  #  half the probabilities of detection will be < 0.5 
  # the other half will be > 0.5.
  # 3. Pick the lower and upper ctil based on quantiles of the distribution
  
  O_star <- build_O_matrix( ranges[O_inds, 2] )
  mu_star <- ranges[mu_inds, 2]
  sigLtil_star <- exp(ranges[sigLtil_inds, 2])
  sigRtil_star <- exp(ranges[sigRtil_inds, 2])
  h_star <- like_neg_ltsgr_cpp(envdat, mu_star, sigLtil_star, sigRtil_star,  O_star)
  ranges[ctil_inds, ] <- -stats::quantile(h_star, rev(quant_vec)) 
  ranges <- ranges[c(mu_inds, sigLtil_inds, sigRtil_inds, ctil_inds, pd_inds, O_inds), ]
  
  return(ranges)
} 
  