#' The negative of the (like) long term stochastich growth function
#' implemented in c++.
#'
#' @param env_dat The environmental data array
#' @param mu A vector of mu
#' @param sigl A vector of sigl
#' @param sigr A vector of sigr
#' @param o_mat An orthogonal matrix
#' @param n_threads The number of threads for parallel computing
#' @returns A numeric value of the log of the likelihood function
#' @export
#'
#' @examples
#' o_mat <- matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' mu <- c(11.433373, 5.046939)
#' sigl <- c(1.036834, 1.556083)
#' sigr <- c(1.538972, 1.458738)
#' M <- like_neg_ltsgr_cpp(
#'   env_dat = envdat_ex,
#'   mu = mu,
#'   sigl = sigl,
#'   sigr = sigl,
#'   o_mat = o_mat
#' )
#' 
like_neg_ltsgr_cpp <- function(env_dat,
                               mu,
                               sigl,
                               sigr,
                               o_mat,
                               n_threads = RcppParallel::defaultNumThreads()) {
  # Set the desired number of threads for RcppParallel operations
  RcppParallel::setThreadOptions(numThreads = n_threads)

  # get various dimensions for convenience
  n <- dim(env_dat)[3] # number of locations with detections or nondetections
  ts_length <- dim(env_dat)[2] # steps in time series
  p <- length(mu) # dimensions/number of env vars

  # subtract mu and apply O to get to u
  env_dat_mat <- matrix(env_dat, nrow = p, ncol = ts_length * n)

  # apply the asymmetries
  if (p == 1) {
    dl_inv <- matrix(1 / sigl, 1, 1)
    dr_inv <- matrix(1 / sigr, 1, 1)
  } else {
    dl_inv <- diag(1 / sigl)
    dr_inv <- diag(1 / sigr)
  }
  drl_inv <- dr_inv - dl_inv

  res <- like_ltsg(
    env_m = env_dat_mat,
    mu = mu,
    dl_mat = dl_inv,
    drl_mat = drl_inv,
    ortho_m = t(o_mat),
    q = ts_length,
    r = n
  )
  
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())

  return(res)
}
