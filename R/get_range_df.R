#' Get range data frame. Creates a unfilled data frame
#' to store the parameter ranges given a number of environmental variables
#' @param env_dat An environmental array with dimenssions given by
#' environmental variables, time series steps and locations.
#' Should provide an environmental array where the detections are positive
#' @param quant_vec A vector of quantiles to limit the range of the
#' parameters given the occurrence in the environmental time series.
#' Default is set to c(0.1, 0.5, 0.9) to center in the median value
#' @returns A data.frame with NA three columns, lower bound, center and
#' upper bound of the range of the parameters.
get_range_df <- function(env_dat, quant_vec = c(0.1, 0.5, 0.9)) {
  checkmate::assert_array(env_dat, min.d = 1, null.ok = FALSE, any.missing = FALSE)
  checkmate::assert_vector(quant_vec, len = 3, strict = TRUE)
  
  p <- dim(env_dat)[1]
  

  ranges <- data.frame(
    lower =  NA * numeric(numparms(p)),
    center = NA * numeric(numparms(p)),
    upper =  NA * numeric(numparms(p))
  )

  q <- (p^2 - p) / 2

  o_inds <- 1:(q)
  mu_inds <- (1 + q):(q + p)
  sigl_inds <- (1 + q + p):(q + 2 * p)
  sigr_inds <- (1 + q + 2 * p):(q + 3 * p)
  pd_inds <- (1 + q + 3 * p):(numparms(p) - 1)
  ctil_inds <- numparms(p)

  rownames(ranges)[o_inds] <- paste0("o_par", 1:q)
  rownames(ranges)[mu_inds] <- paste0("mu", 1:p)
  rownames(ranges)[sigl_inds] <- paste0("sigl", 1:p)
  rownames(ranges)[sigr_inds] <- paste0("sigr", 1:p)
  rownames(ranges)[pd_inds] <- "pd"
  rownames(ranges)[ctil_inds] <- "ctil"

  # get reasonable ranges of mu, sigL and sigR values
  for (counter in 1:p) {
    h <- as.numeric(env_dat[counter, , ])

    # get the mu range
    ranges[mu_inds[counter], ] <- unname(stats::quantile(h, probs = quant_vec))

    # get mu center
    mu_center <- ranges[mu_inds[counter], 2]

    # get the sigL range
    h2 <- sqrt(mean((h[h < mu_center] - mu_center)^2))
    # the log is because we want these on the math scale
    ranges[sigl_inds[counter], ] <- log(c(h2 / 2, h2, 2 * h2))
    # get the sig_r range
    h2 <- sqrt(mean((h[h > mu_center] - mu_center)^2))
    # the log is because we want these on the math scale
    ranges[sigr_inds[counter], ] <- log(c(h2 / 2, h2, 2 * h2))
  }
  # O parameters. This does not matter too much because of the main-to-one
  # nature of the exponential map.
  ranges[o_inds, 1] <- -pi
  ranges[o_inds, 2] <- 0
  ranges[o_inds, 3] <- pi

  # pd parameters
  ranges[pd_inds, ] <- logit(quant_vec)

  ## ctil range.

  # 1. Evaluate lolik in  central values of all the other parameters
  # 2. Pick the central ctil to be the opposite of the median of those values
  # 2.1 For those central parameters half the probabilities of detection
  # will be < 0.5 and the other half will be > 0.5,
  # 3. Pick the lower and upper ctil based on quantiles of the distribution

  o_star <- build_o_matrix(ranges[o_inds, 2])
  mu_star <- ranges[mu_inds, 2]
  sigl_star <- exp(ranges[sigl_inds, 2])
  sigr_star <- exp(ranges[sigr_inds, 2])
  h_star <- like_neg_ltsgr_cpp(env_dat, mu_star, sigl_star, sigr_star, o_star)
  ranges[ctil_inds, ] <- -stats::quantile(h_star, rev(quant_vec))
  ranges <- ranges[c(
    mu_inds, sigl_inds, sigr_inds,
    ctil_inds, pd_inds, o_inds
  ), ]

  ranges
}
