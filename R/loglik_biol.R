#' Logarithm of the likelihood of detection of a species
#' @param env_dat The environmental data array. This is a three dimensional
#' array of dimensions (number of locations of species detection or
#' pseudo-absence) by (time series length) by (number of environmental variables
#' considered).
#' @param occ Presence absence binary vector. Same length as dimension 1 of
#' env_dat.
#' @param mu A vector of mu parameters. Unconstrained real numbers. Same length
#' as dimension 3 of env_dat.
#' @param sigl A vector of sigl parameters
#' @param sigr A vector of sigr parameters
#' @param ctil C tilde parameter
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param o_mat An orthogonal matrix
#' @param sum_log_p Logical (default TRUE) returns the sum of log of probability
#' detection
#' @param return_prob Logical (default FALSE) returns the probability of
#' detections instead the log of probabilities
#' @param num_threads The number of threads for parallel computing
#'
#' @returns The value of the logarithm of the likelihood function in the
#' provided parameters for a given set of environmetnal conditions an presence
#' absence locations
#' @export
#'
#' @examples
#' o_mat <- param_list_example$o_mat
#' mu <- param_list_example$mu
#' sigl <- param_list_example$sigl
#' sigr <- param_list_example$sigr
#' pd <- param_list_example$pd
#' ctil <- param_list_example$ctil
#' env_dat <- envdat_ex
#' occ <- occExample
#'
#' ll <- loglik_biol(
#'   env_dat,
#'   occ,
#'   mu = mu,
#'   sigl = sigr,
#'   sigr = sigl,
#'   ctil = ctil,
#'   pd = pd,
#'   o_mat = o_mat
#' )
#' ll
loglik_biol <- function(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat,
                        num_threads = RcppParallel::defaultNumThreads(),
                        sum_log_p = TRUE,
                        return_prob = FALSE) {
  # Validate inputs for modeling function --------------------------------------
  # occ: must be either a logical vector (TRUE/FALSE) with no NAs or a numeric
  # or integer  vector containing only 0 and 1 with no NA

  #   Using a disjunctive assert so either condition is acceptable
  checkmate::assert(
    checkmate::check_logical(occ, any.missing = FALSE),
    checkmate::check_integerish(occ, lower = 0, upper = 1, any.missing = FALSE),
    .var.name = "occ"
  )

  # env_dat: must be an array with at least 2 dimensions. We consider:
  # locations x time for one environmental variable and
  # location x time x environmental variable 2d and upper dimensions

  # This prevents passing a vector or 1D array by mistake.
  checkmate::assert_array(env_dat, min.d = 2)

  # mu: numeric vector (length >= 1) with no missing values.
  checkmate::assert_numeric(mu, any.missing = FALSE, min.len = 1)

  # sigl: numeric vector (length >= 1) with no missing values. Left side scale
  # of asymmetrical long term stochastic growht function)
  checkmate::assert_numeric(sigl, any.missing = FALSE, min.len = 1)

  # sigl: numeric vector (length >= 1) with no missing values. Right side scale
  # of asymmetrical long term stochastic growht function)
  checkmate::assert_numeric(sigr, any.missing = FALSE, min.len = 1)

  # ctil: single numeric scalar (len == 1) with no missing values. Threshold
  # parameter; enforcing scalar avoids vector to be automatically repeated
  # (recycled) to match the length of longer vectors in operations
  # without warning
  checkmate::assert_numeric(ctil, any.missing = FALSE, len = 1)

  # pd: single numeric scalar (len == 1) with no missing values. Penalty
  # in the probability of detection; enforcing scalar (see above ctil).
  checkmate::assert_numeric(pd, any.missing = FALSE, len = 1)

  # o_mat: numeric matrix with at least 1 row and 1 column and no NAs.
  # Observation/occurrence matrix; dimensions must be valid and no missing.
  checkmate::assert_matrix(
    o_mat,
    min.rows = 1, min.cols = 1, any.missing = FALSE
  )


  # establish the desired number of threads to use. Is set as defaultNumThreads
  RcppParallel::setThreadOptions(numThreads = num_threads)

  # get the probability of detection for each location
  log_p <- log_prob_detect(
    env_dat = env_dat,
    mu = mu,
    sigl = sigl,
    sigr = sigr,
    ctil = ctil,
    pd = pd,
    o_mat = o_mat,
    return_prob = FALSE
  )

  # If sum_log_p is TRUE, the user wants the location-specific log-likelihoods
  # to be summed, otherwise they want them separately as a vector.
  if (sum_log_p) {
    res <- sum(occ * log_p + (1 - occ) * log1mexp(-log_p))
  } else {
    res <- occ * log_p + (1 - occ) * log1mexp(-log_p)
  }

  # If return_prob is TRUE the user wants linear-scale instead of log-scale
  # likelihoods.
  if (return_prob == TRUE) {
    res <- exp(res)
  }

  # Return to the earlier number-of-threads settings
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())

  # Return result
  res
}
