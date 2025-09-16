#' Title
#'
#' @param envdat The environmental data array
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param ctil C tilde parameter 
#' @param O An orthogonal matrix
#' @param num_threads The number of threads for parallel computing
#' @param return_prob Logical (default FALSE) Flag to return
#' probability of detection instead the log of probability of detection
#' @returns A numeric value of the the probability (or log prob) of detection
#' @export
#'
#' @examples
#' mu = c(11.433373, 5.046939)
#' sigLtil = c(1.036834, 1.556083)
#' sigRtil = c(1.538972, 1.458738)
#' ctil = -2
#' pd = 0.9
#' O = matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' M <- logprobdetect(envdat_ex, mu, sigRtil, sigRtil,ctil, pd, O)
logprobdetect = function(envdat, mu, sigLtil, sigRtil, ctil, pd, O,
                         return_prob = FALSE,
                         num_threads = RcppParallel::defaultNumThreads()){
  
  RcppParallel::setThreadOptions(numThreads = num_threads)
  
  h <-  like_neg_ltsgr_cpp(O = O, mu = mu,
                           sigLtil = sigLtil, sigRtil = sigRtil, envdat = envdat)
  
  #Get probability of detection
  #pdetect = pd/(1+exp(ctil+h)) #this is what you want, but you want to compute it originally on the log scale
  #logpdetect = log(pd)-log(1+exp(ctil+h)) #this is what you want on the log scale, but need to use a numerically stable version of log(1+e) 
  logpdetect <- log(pd) - copula::log1pexp(ctil + h)
  
  RcppParallel::setThreadOptions(numThreads = RcppParallel::defaultNumThreads())
  
  
  if(return_prob){
    return(exp(logpdetect))
  } else (
    return(logpdetect)
  )
  
  return(logpdetect)  
}