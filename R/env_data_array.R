#' envDataArray
#' Get an array of environmental data from presence-absence points.
#' @param occ Occurrence data frame. Should contain longitude and latitude
#' columns.
#' @param env_data List of environmental variables time series stack.
#'
#' @return An array of M points times N time steps times P environmental
#' variables
#' @export
#'
#' @examples
#' occ <- mus_virtualis[1:5, ]
#' bio1_ts <- terra::unwrap(cmcc_cm_bio1)
#' bio12_ts <- terra::unwrap(cmcc_cm_bio12)
#' env_data <- list(bio1 = bio1_ts, bio12 = bio12_ts)
#' env_data_array(env_data, occ)
env_data_array <- function(env_data, occ = NULL) {
  if (!is.null(occ)) {
    pts <- terra::vect(occ, geom = c("longitude", "latitude"))

    if (length(env_data) == 1) {
      env_data_array <- terra::extract(
        x = env_data[[1]],
        y = pts,
        cell = FALSE,
        ID = FALSE
      )
      env_data_array <- as.matrix(env_data_array)
    } else {
      env_data_array <- Map(f = \(x) {
        terra::extract(x, pts, cell = FALSE, ID = FALSE)
      }, env_data)
      env_data_array <- Map(f = \(x) {
        stats::setNames(x, paste0(names(x)[[1]], "_", seq_len(ncol(x))))
      }, env_data_array)
      env_data_array <- Map(f = as.matrix, env_data_array)
      env_data_array <- simplify2array(env_data_array)
      # temporary rotate the array to fit with the xsdmMle
      env_data_array <- aperm(env_data_array)
    }
  } else {
    if (length(env_data) == 1) {
      env_data_array <- terra::as.data.frame(env_data[[1]])
      env_data_array <- as.matrix(env_data_array)
    } else {
      env_data_array <- Map(f = \(x) {
        as.matrix(terra::as.data.frame(x))
      }, env_data)
      env_data_array <- Map(f = \(x) {
        stats::setNames(x, paste0(names(x)[[1]], "_", seq_len(ncol(x))))
      }, env_data_array)
      env_data_array <- simplify2array(env_data_array)
      # temporary rotate the array to fit with the xsdmMle
      env_data_array <- aperm(env_data_array)
    }
  }

  return(env_data_array)
}
