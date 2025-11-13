library(testthat)

test_that("loglik_math: positive path mirrors loglik_biol (sign + threads)", {
  
  # --- minimal fixtures (as in test-loglik_biol) ---
  n_loc  <- 2L
  t_len  <- 2L
  n_env  <- 2L
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ     <- c(1, 0)
  
  mu   <- c(1, 1)
  sigl <- c(1, 1)
  sigr <- c(1, 1)
  ctil <- 0.1
  pd   <- 0.5
  o_mat <- diag(n_env)
  
  # --- we need the math <-> bio converter(s) present in the package ---
  skip_if_not(exists("math_to_bio"), "math_to_bio() must exist to run loglik_math()")
  #skip_if_not(exists("bio_to_math"), "bio_to_math() required to build a valid param_vector")
  
  # To do: bio_to_math Build a math-space parameter vector from a known-good 
  # biological list
  # listofpars -> bio_to_math -> param_vector
  
  param_vector <- c(
    mu1    = 1,
    mu2    = 1,
    sigl1  = 1,
    sigl2  = 1,
    sigr1  = 1,
    sigr2  = 1,
    ctil   = 1,
    pd     = 1,
    o_par1 = 1
  )

  
  # By default negative = TRUE, so loglik_math should return a possitive value 
  #of log-lik
  
  expect_silent(ll_math_neg <- loglik_math(param_vector,
                                           env_dat,
                                           occ,
                                           negative = TRUE))
  expect_type(ll_math_neg, "double")
  expect_length(ll_math_neg, 1L)
  expect_true(ll_math_neg > 0)
  # 
  # # Exercise num_threads branch (without asserting on global state)
  expect_silent(loglik_math(param_vector, env_dat, occ, num_threads = 1L))
})

test_that("loglik_math: opt vector fixes parameters (no-ops still run)", {

  n_loc  <- 2L
  t_len  <- 2L
  n_env  <- 2L
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ     <- c(1, 0)

  mu   <- c(1, 1)
  sigl <- c(1, 1)
  sigr <- c(1, 1)
  ctil <- 0.1
  pd   <- 0.5
  o_mat <- diag(n_env)

  skip_if_not(exists("math_to_bio"))

  param_vector <- c(
    mu1    = 13.1,
    mu2    = 5.4,
    sigl1  = 0.9,
    sigl2  = -0.4,
    sigr1  = 0.3,
    sigr2  = -0.5,
    ctil   = -4.4,
    pd     = -1.7,
    o_par1 = -9.5
  )


  # Provide an opt vector that "fixes" everything to the same values (no-op),
  # just to cover the replacement branch safely.
  opt_same <- param_vector

  expect_silent(
    res <- loglik_math(param_vector, env_dat, occ, opt = opt_same, negative = FALSE)
  )
  # Should remain a valid numeric scalar
  expect_true(is.numeric(res) && length(res) == 1L && !is.na(res))
})

test_that("loglik_math: input validation errors on bad env_dat and occ", {

  n_loc  <- 2L
  t_len  <- 2L
  n_env  <- 2L
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ     <- c(1, 0)

  mu   <- c(1, 1)
  sigl <- c(1, 1)
  sigr <- c(1, 1)
  ctil <- 0.1
  pd   <- 0.5
  o_mat <- diag(n_env)


  param_vector <- c(
    mu1    = 13.1,
    mu2    = 5.4,
    sigl1  = 0.9,
    sigl2  = -0.4,
    sigr1  = 0.3,
    sigr2  = -0.5,
    ctil   = -4.4,
    pd     = -1.7,
    o_par1 = -9.5
  )

  # Bad env_dat
  expect_error(loglik_math(param_vector, 1:5, occ), regexp = "env_dat")

  # Bad occ
  expect_error(loglik_math(param_vector, env_dat, c(-1, 0)), regexp = "occ")
})
# 
# Optional safety net: if current implementation validates mu/sigl/... before conversion,
# it will error with 'object .* not found'. Keep until loglik_math moves those checks
# AFTER math_to_bio(); then remove this test.



test_that("loglik_math: (temporary) fails before conversion if bio params are accessed", {
  n_loc   <- 2L
  t_len   <- 2L
  n_env   <- 2L
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ     <- c(1, 0)
  
  param_vector <- 0  # intentionally malformed (length 1)
  
  # Current behavior: early input validation on param_vector
  expect_error(
    loglik_math(param_vector, env_dat, occ),
    regexp = "Assertion on 'param_vector' failed|param_vector.*length",
    fixed = FALSE
  )
})


test_that("loglik_math uses non-NA opt values to override internal param_vector", {
  set.seed(1)
  n_loc  <- 2L
  t_len  <- 2L
  n_env  <- 2L
  env_dat <- array(0, dim = c(n_loc, t_len, n_env))
  occ     <- c(1, 0)
  
  mu   <- c(1, 1)
  sigl <- c(1, 1)
  sigr <- c(1, 1)
  ctil <- 0.1
  pd   <- 0.5
  o_mat <- diag(n_env)
  
  
  param_vector <- c(
    mu1    = 13.1,
    mu2    = 5.4,
    sigl1  = 0.9,
    sigl2  = -0.4,
    sigr1  = 0.3,
    sigr2  = -0.5,
    ctil   = -4.4,
    pd     = -1.7,
    o_par1 = -9.5
  )
  
  # Two opts differing only by mu2 non-NA value
  opt_a <- c(mu1 = NA_real_,
             mu2 = 5,
             sigl1 = NA_real_,
             sigl2 = NA_real_,
             sigr1 = NA_real_,
             sigr2 = NA_real_,
             ctil = NA_real_,
             pd = NA_real_,
             opar_1 = NA_real_
             )
  opt_b <- c(mu1 = NA_real_,
             mu2 = 5.1,
             sigl1 = NA_real_,
             sigl2 = NA_real_,
             sigr1 = NA_real_,
             sigr2 = NA_real_,
             ctil = NA_real_,
             pd = NA_real_,
             opar_1 = NA_real_
  )
  
  ll_a <- loglik_math(occ = occ, env_dat = env_dat, param_vector = param_vector, opt = opt_a)
  ll_b <- loglik_math(occ = occ, env_dat = env_dat, param_vector = param_vector, opt = opt_b)
  
  # If non-NA mu2 is assigned into the param vector, the outputs should differ
  expect_false(isTRUE(all.equal(ll_a, ll_b)))
})

