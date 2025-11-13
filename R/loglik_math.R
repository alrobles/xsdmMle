#' log likelihood orthogonal
#'
#' @param param_vector A vector with parameters to test in a log-likelihood
#' function of the species distribution model.
#' @param env_dat An array with environmental data, includes the time series
#' in each location for each environmental variable
#' @param occ A presence absence vector
#' @param opt An optional vector to mute a specific parameter to evaluate. It is
#' further used to profile the optimization
#' @param num_threads The number of threads to run the main function
#' @param negative Logical. If true returns the negative of the log-likelihood
#' function. Flag should be set when optimize.
#'
#' @returns A value with the log of the likelihood for a given set of
#' parameters, environmental variables and presence-absences occurrences.
#' @export
#'
#' @examples
#' env_dat <- envdat_ex
#' occ <- occExample
#' params <- param_table_example[5, ]
#' param_vector <- params
#' loglik_math(params, env_dat, occ)
#' 
#' opt_a <- c(mu1 = NA_real_,
#'            mu2 = 0,
#'            sigl1 = NA_real_,
#'            sigl2 = NA_real_,
#'            sigr1 = NA_real_,
#'            sigr2 = NA_real_,
#'            ctil = NA_real_,
#'            pd = NA_real_,
#'            opar_1 = NA_real_
#'            )
#' loglik_math(params, env_dat, occ, opt = opt_a)
loglik_math <- function(param_vector,
                        env_dat,
                        occ,
                        opt = NULL,
                        num_threads = RcppParallel::defaultNumThreads(),
                        negative = TRUE) {
  # Set the desired number of threads for RcppParallel operations
  RcppParallel::setThreadOptions(numThreads = num_threads)
  
  
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
  
  # param_vector: numeric vector (length >= 1) with no missing values.
  checkmate::assert_vector(param_vector, any.missing = FALSE, min.len = 5)
  
  # Check fix values in param_vector
  if (is.null(opt)) {
    opt <- rep(NA, length(param_vector))
  }

  # Check if any of opt is not NA
  if (any(!sapply(opt, is.na))) {
    i <- !sapply(opt, is.na)
    # Fix non-NA values
    param_vector[i] <- opt[i]
  }

  
  # Prepare parameters from math scale to biological scale
  param_list <- math_to_bio(param_vector)
  
  # Now validate biological parameters (mu, sigl, sigr, ctil, pd)
  checkmate::assert_numeric(param_list$mu,  any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(param_list$sigl, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(param_list$sigr, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(param_list$ctil, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(param_list$pd,   any.missing = FALSE, len = 1)

  # Function that create loglik_biol ad-hoc function to evaluate passing
  # arguments using do.call
  f <- function(env, occ) {
    function(mu, sigl, sigr, ctil, pd, o_mat) {
      loglik_biol(env, occ, mu, sigl, sigr, ctil, pd, o_mat)
    }
  }
  # Create function to pass param_list arguments by-passing env, occ outside 
  # the list
  f_par <- f(env_dat, occ)
  res <- suppressWarnings(do.call(f_par, args = param_list))

  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  
  # Flag to return negative or positive values. We invert the function 
  # (i. e. returns negative) when want to maximize. 
  # Maximize is the standard behavior of ucminf optimizer We use in this package
  ifelse(!negative, res, -res)
}

