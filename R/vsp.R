#' Virtual species
#'
#' @param envData A list of time serires rasters of bioclimatic variables
#' @param param_list A list of parameters
#'
#' @returns
#' 

vsp <- function(envData, param_list){
  
  envM <- envDataArray(envData)
  
  f <- function(env)function(mu, sigl, sigr, c, pd, O){
    logprobdetect(env, mu, sigl, sigr, c, pd, O)
  }
  if(is.null(param_list)){
    stop("Provide a valid parameter list")
  }
  
  f_par <- f(envM)
  
  coords <- terra::crds(envData[[1]])
  crs_val <- terra::crs(envData[[1]])
  probs <- suppressWarnings(do.call(f_par, args = param_list))
  data.frame(coords, probs)|>
    terra::rast(crs = crs_val)
}
