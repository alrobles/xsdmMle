#' Logarithm of the likelihood oof detection of a species
#' @param env_dat The environmental data array
#' @param occ Presence absence binary vector
#' @param mu A vector of mu parameters
#' @param sigl A vector of sigl parameters
#' @param sigr A vector of sigr parameters
#' @param ctil C tilde parameter
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param o_mat An orthogonal matrix
#' @param sum_logp Logical (default TRUE) returns the sum of log of probability
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
#' ll <- loglik_orthog_nd(
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
loglik_orthog_nd <- function(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat,
                             num_threads = RcppParallel::defaultNumThreads(),
                             sum_logp = TRUE,
                             return_prob = FALSE) {
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
  if (sum_logp) {
    res <- sum(occ * log_p + (1 - occ) * log1mexp(-log_p))
  } else {
    res <- occ * log_p + (1 - occ) * copula::log1mexp(-log_p)
  }

  if (return_prob == TRUE) {
    res <- exp(res)
  }
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  return(res)
}
