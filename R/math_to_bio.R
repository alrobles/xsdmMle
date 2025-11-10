#' Convert a unconstrained vector of parameters in a list of parameters
#' to fit the log likelihood function.
#'
#' @param param_vector A vector of unconstrained parameters to create a list for
#' the likelihood function
#' @returns A list of parameters to pass to the log likelihood function.
#' It contains o_par, mu, sigl, sigr, ctil, pd
#' @export
#'
#' @examples
#' math_to_bio(param_table_example[1, ])
math_to_bio <- function(param_vector) {
  o_par <- param_vector[grep("o_par", names(param_vector))]
  param_list <- list(
    mu = param_vector[grep("mu", names(param_vector))] |>
      as.numeric(),
    sigl = param_vector[grep("sigl", names(param_vector))] |>
      exp() |>
      as.numeric(),
    sigr = param_vector[grep("sigr", names(param_vector))] |>
      exp() |>
      as.numeric(),
    ctil = param_vector[grep("ctil", names(param_vector))] |>
      as.numeric(),
    pd = param_vector[grep("pd", names(param_vector))] |>
      expit() |>
      as.numeric(),
    o_mat = build_o_matrix(o_par)
  )
  param_list
}
