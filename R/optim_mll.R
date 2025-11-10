#' envdat_ex_occ <- envdat_ex[ , , occExample == 1]
#' startparms(envdat_ex_occ)
#'
#' @param envdat Environmental time series array
#' @param pa Presence absence vector
#' @param parallel Parallelization strategy
#' @param numstarts Number of samples to start the optimizations.
#' @export
#' @examples
#' optim_df <- optim_mll(envdat_ex[, , 1:4], occExample[1:4], numstarts = 4)
#'
optim_mll <- function(envdat, pa, parallel = FALSE, numstarts = 100) {
  envdat_ex_occ <- envdat[, , pa == 1]

  param_table <- startparms(envdat_ex_occ, numstarts = numstarts)

  list_of_pars <- split(param_table, seq(nrow(param_table)))

  list_of_pars <- Map(unlist, list_of_pars)
  # function generating of functions
  # useful to pass the enviroinmental parameters and creates a function
  # that catch the parameters in the parallelization

  f_gen <- function(envdat_, pa_) {
    function(params) {
      # minimization with ucminf algorithm.
      # we switch sign with negative = TRUE flag
      suppressWarnings({
        res <- ucminf::ucminf(
          par = params,
          fn = loglik_orthog_nd_unconstr,
          envdat = envdat_,
          pa = pa_,
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
        output <- c(res$par,
          value = -res$value,
          convergence = res$convergence
        )
      })

      return(output)
    }
  }

  f <- f_gen(envdat, pa)

  if (parallel) {
    with(future::plan(future.callr::callr), local = TRUE)

    res <- furrr::future_map(list_of_pars, \(x) f(x),
      .options = furrr::furrr_options(seed = NULL),
      .progress = TRUE
    )
  } else {
    res <- purrr::map(list_of_pars, \(x) f(x),
      .progress = TRUE
    )
  }
  res <- purrr::reduce(res, rbind) |>
    as.data.frame() |>
    tibble::as_tibble()
  res <- res[order(res$value, decreasing = TRUE), ]
  res$index <- 1:nrow(res)
  return(res)
}
