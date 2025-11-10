#' Probability of Detection
#'
#' @param env_dat The environmental data array
#' @param mu A vector of mu
#' @param sigl A vector of sigl
#' @param sigr A vector of sigr
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param ctil C tilde parameter
#' @param o_mat An orthogonal matrix
#' @param num_threads The number of threads for parallel computing
#' @param return_prob Logical (default FALSE) Flag to return
#' probability of detection instead the log of probability of detection
#' @returns A numeric value of the the probability (or log prob) of detection
#' @export
#'
#' @examples
#' mu <- c(11.433373, 5.046939)
#' sigl <- c(1.036834, 1.556083)
#' sigr <- c(1.538972, 1.458738)
#' ctil <- -2
#' pd <- 0.9
#' o_mat <- matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' M <- log_prob_detect(envdat_ex, mu, sigl, sigl, ctil, pd, o_mat)
log_prob_detect <- function(env_dat,
                            mu,
                            sigl,
                            sigr,
                            ctil,
                            pd,
                            o_mat,
                            return_prob = FALSE,
                            num_threads = RcppParallel::defaultNumThreads()) {
  RcppParallel::setThreadOptions(numThreads = num_threads)

  h <- like_neg_ltsgr_cpp(env_dat = env_dat,
                          mu = mu,
                          sigl = sigl,
                          sigr = sigr,
                          o_mat = o_mat)

  # Get probability of detection
  log_p <- log(pd) - log1pexp(ctil + h)
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())

  ifelse(return_prob, exp(log_p), log_p)
}
