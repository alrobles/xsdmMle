#' Title
#'
#' @param optim_vec Vector of optim parameters
#' fitted with optim_ll function
#' @param envdat Environmental dataset
#' @param pa presence absence vector
#' @param parallel Run the profiles in parallel
#' @param stepsize The stepsize of the profiler
#'
#' @returns A data.frame with the profile for each
#' parameter
#' optimVector <- mll_df_example[1, 1:9] |> unlist()
#' profile_optim_par(optimVector, stepsize = 1)
profile_optim_par <- function(optim_vec, envdat, pa, stepsize = 0.001,
                              parallel = FALSE) {
  f_optim_functor <- function(envdat, pa) {
    function(optim_vec,
             parindex = 1,
             stepsize = NULL) {
      if (is.null(stepsize)) {
        stepsize <- 0.001
      }

      nsteps <- 1
      magnitude_index <- -1
      new_optim_vec <- optim_vec

      optim_ll <- loglik_orthog_nd_unconstr(optim_vec,
        envdat,
        pa,
        negative = FALSE
      )
      new_optim_ll <- optim_ll
      # threshold
      alpha_step <- 0.94
      alpha <- 0.95
      degree_freedom <- 1 # nummu1ber of fixed parameters
      incr_down <- stats::qchisq(alpha, degree_freedom) / 2
      incr_step <- stats::qchisq(alpha_step, degree_freedom) / 2

      delta_step <- incr_down - incr_step

      thresh <- optim_ll - incr_down
      optim_vec_output_df <- c(optim_vec, value = optim_ll)
      newstepsize <- stepsize

      cat("fitting step size \n")

      while (new_optim_ll > (optim_ll - delta_step)) {
        if (new_optim_ll > (optim_ll - delta_step)) {
          magnitude_index <- magnitude_index + 1
          newstepsize <- stepsize * 10^(magnitude_index)
          deltapos <- newstepsize * nsteps
        }

        new_optim_vec[parindex] <- new_optim_vec[parindex] + deltapos

        naVec <- rep(NA, 9)
        naVec[parindex] <- new_optim_vec[parindex]

        suppressWarnings({
          new_optim <- optimx::optimx(
            par = optim_vec,
            fn = loglik_orthog_nd_unconstr,
            env = envdat,
            pa = pa,
            opt = naVec,
            hessian = FALSE,
            negative = FALSE,
            control = list(trace = FALSE, maximize = TRUE, kkt = FALSE),
            method = c("ucminf")
          )
        })

        # cat(paste0("new optim: ", new_optim$value, " "))
        new_optim_ll <- new_optim$value
      }
      # reset
      stepsize <- stepsize * 10^(magnitude_index - 1) / 2

      new_optim_ll <- optim_ll
      new_optim_vec <- optim_vec
      nsteps <- 1

      # right size
      cat("start right size \n")
      while (new_optim_ll > thresh) {
        # cat("step ", nsteps, "\n")
        # cat(new_optim_ll, " > ", thresh, "\n")
        # print(new_optim_ll > thresh)

        if (nsteps > 20) {
          break("stop: reach maximum steps")
        }
        deltapos <- nsteps * stepsize
        new_optim_vec[parindex] <- new_optim_vec[parindex] + deltapos

        naVec <- rep(NA, 9)
        naVec[parindex] <- new_optim_vec[parindex]


        suppressMessages({
          new_optim <- optimx::optimx(
            par = new_optim_vec,
            fn = loglik_orthog_nd_unconstr,
            envdat = envdat,
            pa = pa,
            opt = naVec,
            hessian = FALSE,
            negative = FALSE,
            control = list(trace = FALSE, maximize = TRUE, kkt = FALSE),
            method = c("ucminf")
          )
        })

        # delta_ll <- optim_ll - new_optim$value
        new_optim_ll <- new_optim$value
        # threshold
        nsteps <- nsteps + 1
        # print(new_optim_ll)
        new_optim[parindex] <- new_optim_vec[parindex]
        optim_vec_output_df <- rbind(optim_vec_output_df, c(unlist(new_optim[1:9]), value = new_optim_ll))
      }

      # reset

      new_optim_ll <- optim_ll
      new_optim_vec <- optim_vec
      nsteps <- 1

      # left size
      cat("Left side \n")
      while (new_optim_ll > thresh) {

        if (nsteps > 20) {
          break("stop: reach maximum steps")
        }
        deltapos <- nsteps * stepsize
        new_optim_vec[parindex] <- new_optim_vec[parindex] - deltapos

        naVec <- rep(NA, 9)
        naVec[parindex] <- new_optim_vec[parindex]


        suppressMessages({
          new_optim <- optimx::optimx(
            par = new_optim_vec,
            fn = loglik_orthog_nd_unconstr,
            envdat = envdat,
            pa = pa,
            opt = naVec,
            hessian = FALSE,
            negative = FALSE,
            control = list(trace = FALSE, maximize = TRUE, kkt = FALSE),
            method = c("ucminf")
          )
        })

        # delta_ll <- optim_ll - new_optim$value
        new_optim_ll <- new_optim$value
        # threshold
        nsteps <- nsteps + 1

        new_optim[parindex] <- new_optim_vec[parindex]
        optim_vec_output_df <- rbind(
          optim_vec_output_df,
          c(unlist(new_optim[1:9]), value = new_optim_ll)
        )
      }

      rownames(optim_vec_output_df) <- 1:nrow(optim_vec_output_df)

      return(optim_vec_output_df)
    }
  }
  f_test <- f_optim_functor(envdat, pa)


  if (parallel) {
    with(future::plan(future.callr::callr), local = TRUE)

    future::plan(strategy = "multisession", workers = 16)
    res <- furrr::future_map(1:length(optim_vec), \(x){
      f_test(optim_vec, parindex = x)
    },
    .options = furrr::furrr_options(seed = NULL), .progress = TRUE
    )
  } else {
    res <- purrr::map(1:length(optim_vec), \(x){
      f_test(optim_vec, parindex = x)
    }, .progress = TRUE)
  }

  res
}
