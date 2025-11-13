#' Compute Negative Log-Likelihood for Long-Term Stochastic Growth
#'
#' This function calculates the negative of the long-term stochastic growth likelihood
#' using a C++ implementation optimized with \pkg{RcppParallel}. It applies transformations
#' to environmental data and computes the likelihood based on specified parameters.
#'
#' @param env_dat \code{array}  
#'   A 3-dimensional numeric array of environmental data with dimensions:
#'   \itemize{
#'     \item \code{[p]} Number of environmental variables
#'     \item \code{[ts_length]} Time series steps
#'     \item \code{[n]} Number of locations
#'   }
#'   Must not contain missing values.
#'
#' @param mu \code{numeric vector}  
#'   Length-\code{p} vector of location parameters (means) for each environmental variable.
#'
#' @param sigl \code{numeric vector}  
#'   Length-\code{p} vector of left-side standard deviations (asymmetry).
#'
#' @param sigr \code{numeric vector}  
#'   Length-\code{p} vector of right-side standard deviations (asymmetry).
#'
#' @param o_mat \code{matrix}  
#'   A \code{p x p} orthogonal matrix used for rotation in parameter space.
#'
#' @param n_threads \code{integer}  
#'   Number of threads for parallel computation. Defaults to \code{RcppParallel::defaultNumThreads()}.
#'
#' @return \code{numeric scalar}  
#'   The negative log-likelihood value computed from the environmental data and parameters.
#'
#' @details
#' Internally, this function:
#' \enumerate{
#'   \item Reshapes the environmental data into a matrix.
#'   \item Computes inverse matrices for asymmetry adjustments.
#'   \item Calls the C++ function \code{like_ltsg()} for efficient likelihood computation.
#' }
#'
#' @note
#' Ensure that \code{env_dat} has positive detections and no missing values.
#' All parameter vectors (\code{mu}, \code{sigl}, \code{sigr}) must have length equal to the
#' number of environmental variables (\code{p}).
#'
#' @examples
#' # Example usage:
#' o_mat <- matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' mu <- c(11.433373, 5.046939)
#' sigl <- c(1.036834, 1.556083)
#' sigr <- c(1.538972, 1.458738)
#' env_dat <- array(runif(2 * 3 * 2), dim = c(2, 3, 2))
#' like_neg_ltsgr_cpp(env_dat, mu, sigl, sigr, o_mat)
#'
#' @export
like_neg_ltsgr_cpp <- function(env_dat,
                               mu,
                               sigl,
                               sigr,
                               o_mat,
                               n_threads = RcppParallel::defaultNumThreads()) {
  # ---- Assertions ----
  checkmate::assert_array(env_dat, min.d = 2, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_numeric(mu, lower = 0, any.missing = FALSE, len = dim(env_dat)[1])
  checkmate::assert_numeric(sigl, lower = 0, any.missing = FALSE, len = length(mu))
  checkmate::assert_numeric(sigr, lower = 0, any.missing = FALSE, len = length(mu))
  checkmate::assert_matrix(o_mat, nrows = length(mu), ncols = length(mu), any.missing = FALSE)
  checkmate::assert_number(n_threads, lower = 1)
  
  # ---- Set threads ----
  RcppParallel::setThreadOptions(numThreads = n_threads)
  
  # ---- Dimensions ----
  n <- dim(env_dat)[3]       # number of locations
  ts_length <- dim(env_dat)[2] # time steps
  p <- length(mu)            # number of environmental variables
  
  # ---- Reshape env_dat ----
  env_dat_mat <- matrix(env_dat, nrow = p, ncol = ts_length * n)
  
  # ---- Compute inverse matrices ----
  if (p == 1) {
    dl_inv <- matrix(1 / sigl, 1, 1)
    dr_inv <- matrix(1 / sigr, 1, 1)
  } else {
    dl_inv <- diag(1 / sigl)
    dr_inv <- diag(1 / sigr)
  }
  drl_inv <- dr_inv - dl_inv
  
  # ---- Call C++ core function ----
  res <- like_ltsg(
    env_m = env_dat_mat,
    mu = mu,
    dl_mat = dl_inv,
    drl_mat = drl_inv,
    ortho_m = t(o_mat),
    q = ts_length,
    r = n
  )
  
  # ---- Reset threads ----
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  
  return(res)
}