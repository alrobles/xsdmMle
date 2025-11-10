#' log likelihood orthogonal
#'
#' @param param_vector A vector with parameters to test in a log-likelihood
#'  function of the species distribution model.
#' @param env_dat An array with environmental data, includes the time series
#' in each location for each environmental variable
#' @param occ A presence absence vector
#' @param opt An optional vector to mute a specific parameter to evaluate. It is
#' further used to profile the optimization
#' @param negative Logical. If true returns the negative of the log-likelihood
#' function. Flag should be set when optimize.
#' @param num_threads The number of threads to run the main function
#'
#' @returns A value with the log of the likelihood for a given set of
#' parameters, environmental variables and presence-absences ocurrences.
#' @export
#'
#' @examples
#' env_dat <- envdat_ex
#' occ <- occExample
#' params <- param_table_example[5, ]
#' param_vector <- params
#' loglik_orthog_nd_unconstr(params, env_dat, occ)
loglik_orthog_nd_unconstr <- function(param_vector,
                                      env_dat,
                                      occ,
                                      opt = NULL,
                                      negative = TRUE,
                                      num_threads =
                                        RcppParallel::defaultNumThreads()) {
  # Set the desired number of threads for RcppParallel operations
  RcppParallel::setThreadOptions(numThreads = num_threads)

  if (is.null(opt)) {
    opt <- rep(NA, length(param_vector))
  }

  # Check if any of opt is not NA
  if (any(!sapply(opt, is.na))) {
    i <- !sapply(opt, is.na)
    # Fix non-NA values
    param_vector[i] <- opt[i]
  }

  # prep param
  param_list <- math_to_bio(param_vector)


  f <- function(env, occ) {
    function(mu, sigl, sigr, ctil, pd, o_mat) {
      loglik_orthog_nd(env, occ, mu, sigl, sigr, ctil, pd, o_mat)
    }
  }
  f_par <- f(env_dat, occ)
  res <- suppressWarnings(do.call(f_par, args = param_list))

  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  ifelse(!negative, res, -res)
}
