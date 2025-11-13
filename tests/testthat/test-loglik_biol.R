library(testthat)
test_that("loglik_biol: full coverage including uncovered lines", {
  # Minimal valid data with n_env = 2 (matches internal checks)
  n_loc <- 2
  t_len <- 2
  n_env <- 2
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ <- c(1, 0)
  # length matches n_env
  mu <- c(1, 1)
  sigl <- c(1, 1)
  sigr <- c(1, 1)
  ctil <- 0.1
  pd <- 0.5
  # 2 x 2 orthogonal matrix
  o_mat <- diag(n_env)

  # ---- Positive case: covers input validation and default thread options ----
  expect_silent(
    loglik_biol(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat)
  )

  # ---- Covers num_threads argument (thread options lines) ----
  expect_silent(
    loglik_biol(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat, num_threads = 1L)
  )

  # ---- Covers sum_log_p = FALSE branch ----
  res_vec <- loglik_biol(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat,
    sum_log_p = FALSE, return_prob = FALSE
  )
  expect_length(res_vec, length(occ))


  # ---- Covers return_prob = TRUE branch ----
  res_prob <- loglik_biol(env_dat,
    occ,
    mu,
    sigl,
    sigr,
    ctil,
    pd,
    o_mat,
    sum_log_p = TRUE,
    return_prob = TRUE
  )

  expect_true(length(res_prob) == 1 && is.numeric(res_prob) && !is.na(res_prob))


  # ---- Negative cases: trigger each assertion error ----
  expect_error(
    loglik_biol(1:5, occ, mu, sigl, sigr, ctil, pd, o_mat),
    regexp = "env_dat"
  )
  expect_error(
    loglik_biol(env_dat, c(-1, 0), mu, sigl, sigr, ctil, pd, o_mat),
    regexp = "occ"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu = NA_real_, sigl, sigr, ctil, pd, o_mat),
    regexp = "mu"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu, sigl = NA_real_, sigr, ctil, pd, o_mat),
    regexp = "sigl"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu, sigl, sigr = NA_real_, ctil, pd, o_mat),
    regexp = "sigr"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu, sigl, sigr, ctil = c(0.1, 0.2), pd, o_mat),
    regexp = "ctil"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu, sigl, sigr, ctil, pd = c(0.9, 0.8), o_mat),
    regexp = "pd"
  )
  expect_error(
    loglik_biol(env_dat, occ, mu, sigl, sigr, ctil, pd, o_mat = 1:4),
    regexp = "o_mat"
  )
})
