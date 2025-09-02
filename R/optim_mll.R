#' envdat_ex_occ <- envdat_ex[ , , occExample == 1] 
#' startparms(envdat_ex_occ)
#' 
#' @param envdat Environmental time series array 
#' @param pa Presence absence vector
#' @param parallel Parallelization strategy
#' @param numstarts Number of samples to start the optimizations. 
#' @export
#' @examples 
#' optim_df <- optim_mll(envdat_ex[ , ,1:5], occExample[1:5], numstarts = 4)
#' 
optim_mll <- function(envdat, pa, parallel = FALSE, numstarts = 100){
  
  
  envdat_ex_occ <- envdat[ , , pa == 1] 
 
  paramTable <-  startparms(envdat_ex_occ, numstarts = numstarts)
  #paramTable <- paramTableExample[ 1:5,]
  

  list_of_pars <- split(paramTable, seq(nrow(paramTable)))
  #list_of_pars_test <- split(paramTable_test, seq(nrow(paramTable)))
  
  list_of_pars <- Map(unlist, list_of_pars)
  #function generating of functions
  # useful to pass the enviroinmental parameters and creates a function
  # that catch the parameters in the parallelization
  f_gen <- function(envdat_, pa_)function(params){
    #minimization with ucminf algorithm.
    #we switch sign with negative = TRUE flag
    
    suppressWarnings({
    res <- ucminf::ucminf(par = params,
                         fn  = loglik_orthog_nd_unconstr,
                         envdat = envdat_,
                         pa  = pa_,
                         negative = TRUE,
                         num_threads = 4,
                         hessian = FALSE)
    })
    
    output <- c(res$par, value = -res$value, convergence = res$convergence)
    return(output)
    
  }
  
  f <- f_gen(envdat, pa)
  
  if(parallel){
    with(future::plan(strategy = "multisession", workers = 16), local = TRUE)

    #future::plan(strategy = "multisession", workers = 16)
    res <- furrr::future_map(list_of_pars, \(x) f(x),
                                 .options = furrr::furrr_options(seed = NULL),
                                 .progress = TRUE)
    
  } else {
    res <- purrr::map(list_of_pars, \(x) f(x),
                          .progress = TRUE )

  }
  res <- purrr::reduce(res, rbind) |>
    as.data.frame() |>
    tibble::as_tibble()
  
  res <- res[order(res$value, decreasing = TRUE), ]
  res$index <- 1:nrow(res)

  return(res)
}
