#' envDataArray
#' Get an array of environmental data from presence-absence points.
#' @param occ Occurence data frame. Should contain longitude and latitude columns
#' @param envData List of environmental variables time series stack.
#'
#' @return An array of M points times N time steps times P environmental variables
#' @export
#'
#' @examples
#' occ <- mus_virtualis[1:5, ]
#' bio1_ts <- terra::unwrap(cmcc_cm_bio1)
#' bio12_ts <- terra::unwrap(cmcc_cm_bio12)
#' envData <- list(bio1 = bio1_ts, bio12 = bio12_ts)
#' envDataArray(envData, occ)
envDataArray <- function(envData, occ = NULL){
  
  if(!is.null(occ)){
    
    pts <- terra::vect(occ, geom = c("longitude", "latitude"))
    
    if(length(envData) == 1){
      envDataArray <- terra::extract(envData[[1]], pts, cell = FALSE, ID = FALSE)
      envDataArray <- as.matrix(envDataArray)
    } else {
      envDataArray <- Map(f = \(x){terra::extract(x, pts, cell = FALSE, ID = FALSE)}, envData)
      envDataArray <- Map(f = \(x){stats::setNames(x, paste0(names(x)[[1]], "_", 1:ncol(x)))}, envDataArray)
      envDataArray <- Map(f = as.matrix, envDataArray)
      envDataArray <- simplify2array(envDataArray)
      #temporary rotate the array to fit with the xsdmMle
      envDataArray <- aperm(envDataArray)
    }
    
  } else {
    
    if(length(envData) == 1){
      envDataArray <- terra::as.data.frame(envData[[1]])
      envDataArray <- as.matrix(envDataArray)
    } else{
      envDataArray <- Map(f = \(x){as.matrix(terra::as.data.frame(x))}, envData)
      envDataArray <- Map(f = \(x){stats::setNames(x, paste0(names(x)[[1]], "_", 1:ncol(x)))}, envDataArray)
      envDataArray <- simplify2array(envDataArray)
      #temporary rotate the array to fit with the xsdmMle
      envDataArray <- aperm(envDataArray)
    }
    
    
  }
  
  
  
  return(envDataArray)
}

