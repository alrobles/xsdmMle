#' Get range data frame.
#' 
#' Creates a unfilled data frame to store the parameter ranges 
#' given a number of environmental variables
#' 
#' 
#' @param envdat An environmental array with dimenssions P x M x N, where
#'  P is the number of environmental variables, M the length of the time series
#'  and N the number of locations.
#' ?
#' Should provide an environmental array where the detections are possitive
#' ?
#'
#' @param quant_vec A vector of quantiles to limit the range of the
#'  parameters given the occurrence in the environmental time serires.
#'  Default is set to c(0.1, 0.5, 0.9) to center in the median value
#' @returns A data.frame with NA three columns, lower bound, center and
#'  upper bound of the range of the parameters.
guess_parameters_range <- function(envdat, quant_vec = c(0.1, 0.5, 0.9) ){
  p <- dim(envdat)[1]
  
  ranges <- data.frame(
    lower  = NA * numeric(numparms(p)),
    center = NA * numeric(numparms(p)),
    upper  = NA * numeric(numparms(p))
  )

  # number of params for orthogonal  
  q <- (p^2 - p) / 2
  
  # indices of parameters
  ortho_inds <- seq(1, q)
  mu_inds <- seq(1 + q, q + p)
  sigLtil_inds <- seq(1 + q + p, q + 2 * p)
  sigRtil_inds <- seq(1 + q + 2 * p, q + 3 * p)
  pd_inds <- seq(1 + q + 3 * p, numparms(p) - 1)
  ctil_inds <- numparms(p)
  
  rownames(ranges)[ortho_inds] <- paste0("ortho", seq_len(q))
  rownames(ranges)[mu_inds] <- paste0("mu", seq_len(p))
  rownames(ranges)[sigLtil_inds] <- paste0("sigLtil",  seq_len(p))
  rownames(ranges)[sigRtil_inds] <- paste0("sigRtil",  seq_len(p))
  rownames(ranges)[pd_inds] <- "pd"
  rownames(ranges)[ctil_inds] <- "ctil"
  
  # infer reasonable ranges of mu, sigL and sigR values
  for (counter in seq_len(p)) {
    h <- as.numeric(envdat[counter, , ])
    
    # mu range
    ranges[mu_inds[counter], ] <- unname(stats::quantile(h, probs = quant_vec))
    
    # mu center
    mu_center <- ranges[mu_inds[counter], "center"]
    
    # sigL range
    h2 <- sqrt( mean( ( h[h < mu_center] - mu_center )^2 ) )
    # log because we want these on the math (?) scale
    ranges[sigLtil_inds[counter], ] <- log(c(h2/2, h2, 2*h2))
    
    # sigR range
    h2 <- sqrt( mean( ( h[h > mu_center] - mu_center )^2 ) )
    # log because we want these on the math (?) scale
    ranges[sigRtil_inds[counter], ] = log(c(h2/2, h2, 2*h2))
    
  }
  
  # ortho range
  # This does not matter too much because of the main-to-one
  # nature of the exponential map.
  ranges[ortho_inds, 1] <- -pi
  ranges[ortho_inds, 2] <- 0
  ranges[ortho_inds, 3] <- pi
  
  # pd range, logit scaled
  ranges[pd_inds, ] <- logit(quant_vec)
  
  # ctil range
  # 1. Evaluate lolik in central values of all the other parameters
  # 2. Pick the central ctil to be the opposite of the median of those values.
  #    For those central parameters:
  #      Half the probabilities of detection will be < 0.5 
  #      The other half will be > 0.5.
  # 3. Pick the lower and upper ctil based on quantiles of the distribution
  ortho_star <- build_ortho_matrix( ranges[ortho_inds, "center"] )
  mu_star <- ranges[mu_inds, "center"]
  sigLtil_star <- exp(ranges[sigLtil_inds, "center"])
  sigRtil_star <- exp(ranges[sigRtil_inds, "center"])
  h_star <- like_neg_ltsgr_cpp(envdat, mu_star, sigLtil_star, sigRtil_star,  ortho_star)
  ranges[ctil_inds, ] <- -stats::quantile(h_star, rev(quant_vec)) 
  ranges <- ranges[c(mu_inds, sigLtil_inds, sigRtil_inds, ctil_inds, pd_inds, ortho_inds), ]
  
  return(ranges)
} 
  