#' Virtual species
#'
#' @param env_data A list of time series rasters of bioclimatic variables
#' @param param_list A list of parameters
#'
#' @returns A raster with a virtual species probability of detection
vsp <- function(env_data, param_list) {
  env_m <- env_data_array(env_data)

  f <- function(env) {
    function(mu, sigl, sigr, c, pd, o_mat) {
      log_prob_detect(env, mu, sigl, sigr, c, pd, o_mat)
    }
  }
  if (is.null(param_list)) {
    stop("Provide a valid parameter list")
  }

  f_par <- f(env_m)

  coords <- terra::crds(env_data[[1]])
  crs_val <- terra::crs(env_data[[1]])
  probs <- suppressWarnings(
    do.call(
      f_par,
      args = param_list
    )
  )
  probs <- exp(probs)
  data.frame(coords, probs) |>
    terra::rast(crs = crs_val)
}
