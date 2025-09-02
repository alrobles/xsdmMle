#' Title
#'
#' @param O An orthogonal matrix
#' @param mu A vector of mu
#' @param sigLtil A vector of sigl
#' @param sigRtil A vector of sigr
#' @param envdat The environmental data array
#'
#' @returns A numeric value of the log of the likelihood function
#' @export
#'
#' @examples
#' O = matrix(c(-0.4443546, 0.8958510, -0.8958510, -0.4443546), ncol = 2)
#' mu = c(11.433373, 5.046939)
#' sigLtil = c(1.036834, 1.556083)
#' sigRtil = c(1.538972, 1.458738)
#' M <- like_neg_ltsgr_r(O, mu, sigLtil, sigRtil, envdat_ex)

like_neg_ltsgr_r = function(O, mu, sigLtil, sigRtil, envdat)
{
  #get various dimensions for convenience
  n_old = dim(envdat)[3] #number of locations with detections or nondetections
  #n_new = dim(envdat_new)[1]
  tslen = dim(envdat)[2] #steps in time series
  p = length(mu) #dimensions/number of env vars

  #subtract mu and apply O to get to u
  envdat_mat_old = matrix(envdat, nrow = p, ncol = tslen*n_old)
  #envdat_mat_new = matrix(aperm(envdat_new), nrow = p, ncol = tslen*n_old)
  u_old = t(O) %*% (envdat_mat_old - matrix(mu, p, tslen*n_old))
  #u_new = t(O) %*% (envdat_mat_new - matrix(mu, p, tslen*n_new))


  #apply the asymmetries
  if (p==1)
  {
    DLinv = matrix(1/sigLtil,1,1)
    DRinv = matrix(1/sigRtil,1,1)
  } else {
    DLinv = diag(1/sigLtil)
    DRinv = diag(1/sigRtil)
  }


  uasym_old = (DLinv %*% u_old + (DRinv - DLinv) %*% matrix(pmax(0,u_old), p, tslen*n_old) )^2


  #compute the quantity which is related to the ltsgr for each location and return
  res = matrix(apply(FUN = sum, X = uasym_old, MARGIN=2), tslen, n_old)
  res = 0.5*apply(FUN = mean,   X = res, MARGIN = 2)

  return(res)

}
