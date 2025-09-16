#' uasym
#'
#' @param envdat The environmental data array
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr
#' @param O An orthogonal matrix
#' @param num_threads The number of threads for parallel computing
#' @returns A numeric value of the log of the likelihood function
#' @export
#'
#' @examples
#' O = matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' mu = c(11.433373, 5.046939)
#' sigLtil = c(1.036834, 1.556083)
#' sigRtil = c(1.538972, 1.458738)
#' M <- like_neg_ltsgr_cpp(envdat_ex, mu, sigLtil, sigLtil, O)

like_neg_ltsgr_cpp = function(envdat, mu, sigLtil, sigRtil, O, num_threads = RcppParallel::defaultNumThreads())
{
  
  # Set the desired number of threads for RcppParallel operations
  RcppParallel::setThreadOptions(numThreads = num_threads)
  
  #get various dimensions for convenience
  n = dim(envdat)[3] #number of locations with detections or nondetections
  tslen = dim(envdat)[2] #steps in time series
  p = length(mu) #dimensions/number of env vars
  
  #subtract mu and apply O to get to u
  envdat_mat = matrix(envdat, nrow = p, ncol = tslen*n)
  
  #apply the asymmetries
  if (p==1)
  {
    DLinv = matrix(1/sigLtil,1,1)
    DRinv = matrix(1/sigRtil,1,1)
  } else {
    DLinv = diag(1/sigLtil)
    DRinv = diag(1/sigRtil)
  }
  DRLinv <- DRinv -DLinv 
  
  res =  likeLtsg(
    envM = envdat_mat,
    mu = mu,
    DL = DLinv,
    DRL = DRLinv,
    orthoM = t(O),
    q = tslen,
    r = n ) 
  
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  
  return(res)
  
}
