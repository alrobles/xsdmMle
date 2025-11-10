#' Optimization of the log-likelihood function
#' @param envdat Environmental time series array
#' @param occ Presence absence vector
#' @param parallel Parallelization strategy
#' @param numstarts Number of samples to start the optimizations.
#' @export
#' @examples
#' optim_df <- optim_mll(envdat_ex[, , 1:5], occExample[1:5], numstarts = 5)
#'
optim_mll <- function(envdat, occ, parallel = FALSE, numstarts = 100) {
  envdat_ex_occ <- envdat[, , occ == 1]

  params_table <- startparms(envdat_ex_occ, numstarts = numstarts)
  list_of_pars <- split(params_table, seq_len(nrow(params_table)))
  list_of_pars <- Map(unlist, list_of_pars)
  # function generating of functions
  # useful to pass the environmental parameters and creates a function
  # that catch the parameters in the parallelization

  f_gen <- function(envdat_, occ_) {
    function(params) {
      # minimization with ucminf algorithm.
      # we switch sign with negative = TRUE flag
      suppressWarnings({
        res <- ucminf::ucminf(
          par = params,
          fn = loglik_orthog_nd_unconstr,
          env_dat = envdat_,
          occ = occ_,
          negative = TRUE,
          num_threads = RcppParallel::defaultNumThreads() %/% 4,
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
  f <- f_gen(envdat_ = envdat, occ_ = occ)
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
