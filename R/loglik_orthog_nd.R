#
#' Logarithm of the likelihood oof detection of a species
#' @param envdat The environmental data array
#' @param occ Presence absence binary vector
#' @param mu A vector of mu
#' @param sigl A vector of sigl
#' @param sigr A vector of sigr
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
#' o_mat <- paramListExample$O
#' mu <- paramListExample$mu
#' sigl <- paramListExample$sigLtil
#' sigr <- paramListExample$sigRtil
#' pd <- paramListExample$pd
#' ctil <- paramListExample$ctil
#' envdat <- envdat_ex
#' occ <- occExample
#'
#' ll <- loglik_orthog_nd(envdat_ex, occ,
#'   mu = mu,
#'   sigl = sigl,
#'   sigr = sigl,
#'   ctil = ctil, pd = pd, o_mat = o_mat
#' )
#' ll
loglik_orthog_nd <- function(envdat, occ, mu, sigl, sigr, ctil, pd, o_mat,
                             num_threads = RcppParallel::defaultNumThreads(),
                             sum_logp = TRUE,
                             return_prob = FALSE) {
  RcppParallel::setThreadOptions(numThreads = num_threads)

  # get the probability of detection for each location
  logpdetect <- logprobdetect(
    envdat = envdat,
    mu = mu,
    sigl = sigl,
    sigr = sigr,
    ctil = ctil,
    pd = pd,
    o_mat = o_mat,
    return_prob = FALSE
  )
  if (sum_logp) {
    res <- sum(occ * logpdetect + (1 - occ) * copula::log1mexp(-logpdetect))
  } else {
    res <- occ * logpdetect + (1 - occ) * copula::log1mexp(-logpdetect)
  }

  if (return_prob == TRUE) {
    res <- exp(res)
  }

  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  return(res)
}
