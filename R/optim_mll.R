#' Optimize Multivariate Log-Likelihood For Species Distribution Model
#'
#' Performs optimization of a multivariate log-likelihood function using the
#' \pkg{ucminf} algorithm. Multiple random starting points are used to reduce
#' the risk of local optima. Parallelization is supported for faster execution.
#'
#' @param env_dat A 3D numeric array of environmental time series data.
#' @param occ A logical or numeric vector indicating presence or absence (1/0).
#' @param parallel Logical; if \code{TRUE}, optimization runs in parallel using
#' \pkg{furrr} and \pkg{future}. Defaults to \code{FALSE}.
#' @param numstarts Integer; number of random starting points for optimization.
#' Defaults to \code{100}.
#'
#' @return A tibble with optimized parameter sets and their corresponding
#' log-likelihood values, sorted in decreasing order of likelihood.
#' Columns include:
#' \itemize{
#'   \item Estimated parameters.
#'   \item \code{value}: the optimized log-likelihood.
#'   \item \code{convergence}: convergence code from \pkg{ucminf}.
#'   \item \code{index}: rank of the solution.
#' }
#'
#' @details
#' The optimization uses \code{ucminf::ucminf()} with central differences for
#' gradients and relaxed tolerances for robustness. Parallelization uses
#' \pkg{future} and \pkg{furrr} when \code{parallel = TRUE}.
#'
#' @note
#' Presence-absence filtering is applied internally to \code{envdat} before
#' optimization.
#'
#' @examples
#' # Example with subset of environmental data and occurrence vector
#' optim_df <- optim_mll(envdat_ex[, , 1:5], occExample[1:5], numstarts = 5)
#'
#' @seealso \code{\link[ucminf]{ucminf}}, \code{\link[furrr]{future_map}}
#' @export
optim_mll <- function(env_dat, occ, parallel = FALSE, numstarts = 100) {
  # env_dat: must be an array with at least 2 dimensions. We consider:
  # locations x time for one environmental variable and
  # location x time x environmental variable 2d and upper dimensions
  # This prevents passing a vector or 1D array by mistake.
  checkmate::assert_array(env_dat, min.d = 2)
  checkmate::assert(
    checkmate::check_logical(occ, any.missing = FALSE, min.len = 1),
    checkmate::check_integerish(occ,
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      min.len = 1
    ),
    .var.name = "occ"
  )

  env_dat_ex_occ <- env_dat[, , occ == 1]

  # Get parameters to input in model, given environment of occurrence data
  params_table <- start_parms(env_dat = env_dat_ex_occ, num_starts = numstarts)
  list_of_pars <- split(params_table, seq_len(nrow(params_table)))
  list_of_pars <- Map(unlist, list_of_pars)

  # function generating of functions
  # useful to pass the environmental parameters and creates a function
  # that catch the parameters in the parallelization

  f_gen <- function(env_dat_, occ_) {
    function(params) {
      # minimization with ucminf algorithm.
      # we switch sign with negative = TRUE flag
      suppressWarnings({
        res <- ucminf::ucminf(
          par = params,
          fn = loglik_math,
          env_dat = env_dat_,
          occ = occ_,
          negative = TRUE,
          num_threads = RcppParallel::defaultNumThreads(),
          control = list(
            grtol = 1e-4, # Looser gradient tolerance
            xtol = 1e-8, # Looser parameter tolerance
            maxeval = 1000, # More function evaluations
            grad = "central" # Use central differences if analytical grad fails
          ),
          hessian = FALSE
        )
        output <- c(res$par, value = -res$value, convergence = res$convergence)
      })
      output
    }
  }
  f <- f_gen(env_dat_ = env_dat, occ_ = occ)
  if (parallel) {
    with(future::plan(future.callr::callr), local = TRUE)
    res <- furrr::future_map(list_of_pars, \(x) f(x),
      .options = furrr::furrr_options(seed = NULL),
      .progress = TRUE
    )
  } else {
    res <- purrr::map(list_of_pars, \(x) f(x), .progress = TRUE)
  }
  res <- purrr::reduce(res, rbind) |>
    as.data.frame() |>
    tibble::as_tibble()
  res <- res[order(res$value, decreasing = TRUE), ]
  res$index <- seq_len(nrow(res))
  res
}
