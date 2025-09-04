#' Title
#'
#' @param bparms  A list with parameters
#' to adjust to a canonical form
#'
#' @returns A list  with parameters in canonical form
#' 
#' @export
#'
#' @examples
#' 
#' paramTableExample[1, ] |> 
#' math_to_bio() |>
#'  make_cannonical()
make_cannonical <-  function(bparms)
{
  #extract the components that need to be changed
  O <- bparms$O
  sigLtil <- bparms$sigLtil
  sigRtil <- bparms$sigRtil
  
  #first multiply each column of O by 1 or -1 to make the top-most non-zero entry
  #be positive
  for (cc in 1:(dim(O)[2]))
  {
    h = O[,cc]
    firstnzind = min(which(h != 0))
    if (h[firstnzind] < 0)
    {
      O[ , cc] = -h
      h = sigLtil[cc] # switch the corresponding entries 
                      # of sigLtil and sigRtil
                      # to pay the price 
      sigLtil[cc] = sigRtil[cc]
      sigRtil[cc] = h 
    }
  }
  
  #now reorder the columns of O using dictionary order, and bring the entries of
  #sigLtil and sigRtil "along for the ride"
  inds = do.call(order, as.data.frame(t(O)))
  O = O[,inds]
  sigLtil = sigLtil[inds]
  sigRtil = sigRtil[inds]
  
  #reinsert the three altered components and return  
  bparms$O = O
  bparms$sigLtil = sigLtil
  bparms$sigRtil = sigRtil
  return(bparms)
}
