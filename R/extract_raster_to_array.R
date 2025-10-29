#' Extract environmental data
#'
#' Extract an array of environmental data from presence-absence points.
#' 
#' When the raster of environemntal variables has multiple layers, 
#' values from all layers are extracted.
#' @param envData Raster of environmental variables time series stack.
#' @param occ Occurence data frame. Should contain longitude and latitude columns
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
extract_raster_to_array <- function(envData, occ = NULL){

  stopifnot(all(c("longitude", "latitude") %in% colnames(occ)))
  
  if(!is.null(occ)){
    # if occ is passed, extract from its coordinates

    # spatVect from data.frame    
    pts <- terra::vect(
      occ,
      geom = c("longitude", "latitude"),
      crs = "+proj=longlat +datum=WGS84 +no_defs" # assuming EPSG:4326
    )

    # reproject pts if needed
    if (!same.crs(envData, pts)) {
      pts <- project(pts, crs(envData))
    }
    
    if(length(envData) == 1) {

      # extract the one layer
      envDataArray <- terra::extract(envData[[1]], pts, cell = FALSE, ID = FALSE)
      envDataArray <- as.matrix(envDataArray)

    } else {

      # extract all layers
      # not sure what Map does, but this can be handle by terra natively. Maybe faster with Map?
      envDataArray <- Map(f = \(x){terra::extract(x, pts, cell = FALSE, ID = FALSE)}, envData)
      envDataArray <- Map(f = \(x){stats::setNames(x, paste0(names(x)[[1]], "_", 1:ncol(x)))}, envDataArray)
      envDataArray <- Map(f = as.matrix, envDataArray)
      envDataArray <- simplify2array(envDataArray)

      #temporary rotate the array to fit with the xsdmMle
      envDataArray <- aperm(envDataArray)

    }
    
  } else {

    # if occ is not passed, extract all cells
    
    if(length(envData) == 1){

      # extract the one layer
      envDataArray <- terra::as.data.frame(envData[[1]])
      envDataArray <- as.matrix(envDataArray)

    } else{

      # extract all layers
      envDataArray <- Map(f = \(x){as.matrix(terra::as.data.frame(x))}, envData)
      envDataArray <- Map(f = \(x){stats::setNames(x, paste0(names(x)[[1]], "_", 1:ncol(x)))}, envDataArray)
      envDataArray <- simplify2array(envDataArray)
      #temporary rotate the array to fit with the xsdmMle
      envDataArray <- aperm(envDataArray)

    }
    
    
  }
  
  return(envDataArray)
}

