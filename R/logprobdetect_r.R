#' Title
#' @param envdat The environmental data array
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr
#' @param ctil C tilde parameter 
#' @param pd Probability of detection. Parameter between 0 and 1
#' @param O An orthogonal matrix

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
#' M <- logprobdetect_r( envdat_ex,  mu, sigLtil, sigRtil, ctil, pd, O )
logprobdetect_r = function(envdat, mu, sigLtil, sigRtil, ctil, pd, O){
  
  h <-  like_neg_ltsgr_r(envdat, mu, sigLtil, sigRtil, O)
  
  #Get probability of detection
  #pdetect = pd/(1+exp(ctil+h)) #this is what you want, but you want to compute it originally on the log scale
  #logpdetect = log(pd)-log(1+exp(ctil+h)) #this is what you want on the log scale, but need to use a numerically stable version of log(1+e) 
  logpdetect <-  log(pd) - copula::log1pexp(ctil + h)
  
  return(logpdetect)  
}