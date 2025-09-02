#' Title
#'
#' @param O An orthogonal matrix
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param ctil C tilde parameter 
#' @param envdat The environmental data array
#' @returns A numeric value of the log of the probability of detection
#' @export
#'
#' @examples
#' O = matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' mu = c(11.433373, 5.046939)
#' sigLtil = c(1.036834, 1.556083)
#' sigRtil = c(1.538972, 1.458738)
#' pd = 0.9
#' ctil = -1
#' M <- logprobdetect_r(O, mu, sigLtil, sigRtil, pd, ctil, envdat_ex)
logprobdetect_r = function(O, mu, sigLtil, sigRtil, pd, ctil, envdat){
  h <-  like_neg_ltsgr_r(O, mu, sigLtil, sigRtil, envdat)
  
  #Get probability of detection
  #pdetect = pd/(1+exp(ctil+h)) #this is what you want, but you want to compute it originally on the log scale
  #logpdetect = log(pd)-log(1+exp(ctil+h)) #this is what you want on the log scale, but need to use a numerically stable version of log(1+e) 
  logpdetect <-  log(pd) - copula::log1pexp(ctil + h)
  
  return(logpdetect)  
}