#
#' Logarithm of the likelihood oof detection of a species
#' @param envdat The environmental data array
#' @param pa Presence absence binary vector 
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr 
#' @param ctil C tilde parameter 
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param O An orthogonal matrix
#' @param sum_logp Logical (default TRUE) returns the sum of log of probability 
#' detection
#' @param return_prob Logical (default FALSE) returns the probability of detections
#' instead the log of proabilities
#' @param num_threads The number of threads for parallel computing
#'
#' @returns The value of the logarithm of the likelihood function in the 
#' provided parameters for a given set of environmetnal conditions an presence
#' absence locations
#' @export
#'
#' @examples
#' O <-  paramListExample$O
#' mu <- paramListExample$mu
#' sigLtil <- paramListExample$sigLtil
#' sigRtil <- paramListExample$sigRtil
#' pd <- paramListExample$pd
#' ctil <- paramListExample$ctil
#' envdat <- envdat_ex
#' pa <- occExample
#' 
#' ll <- loglik_orthog_nd(envdat_ex, pa , mu = mu, sigLtil = sigLtil,
#' sigRtil =  sigLtil, ctil = ctil, pd = pd, O = O)  
#' 
loglik_orthog_nd = function(envdat, pa, mu, sigLtil, sigRtil, ctil, pd, O, 
                            num_threads = RcppParallel::defaultNumThreads(), 
                            sum_logp = TRUE, 
                            return_prob = FALSE)
{
  RcppParallel::setThreadOptions(numThreads = num_threads)
  
  #get the probability of detection for each location
  logpdetect = logprobdetect(envdat = envdat,
                             mu = mu,
                             sigLtil = sigLtil,
                             sigRtil = sigRtil,
                             pd = pd,
                             ctil = ctil,
                             O = O,
                             return_prob = FALSE
                             )
  
  #now get the log likelihood and return
  #res = sum(pa*logpdetect+(1-pa)*log(1-exp(logpdetect)))
  #this is what you want but you probably want to use
  #a log1mexp type function for numeric stability
  
  
  
  #copula::log1mexp(a) for positive a is actually log(1-exp(-a)) 
  #for some reason 
  
  if(sum_logp){
    res = sum(pa*logpdetect + (1 - pa)*copula::log1mexp(-logpdetect))
  } else {
    res = pa*logpdetect + (1 - pa)*copula::log1mexp(-logpdetect)
  }
  
  if(return_prob == TRUE){
    res <- exp(res)
  }
  
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  return(res)
}
