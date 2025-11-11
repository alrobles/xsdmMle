library(testthat)

test_that("like_neg_ltsgr_cpp covers p == 1 branch with real like_ltsg", {
  # Prepare inputs for p = 1
  env_dat <- array(runif(1 * 4 * 3), dim = c(1, 4, 3)) # p=1, ts_length=4, n=3
  mu <- 0.7
  sigl <- 1.1
  sigr <- 1.3
  o_mat <- matrix(1, nrow = 1, ncol = 1)
  
  # Call the actual function
  result <- like_neg_ltsgr_cpp(env_dat, mu, sigl, sigr, o_mat, n_threads = 1)
  
  # Assertions
  expect_type(result, "double")
  expect_equal(length(result), dim(env_dat)[3]) # matches n
  expect_true(all(is.finite(result)))           # no NA or Inf
  expect_true(all(result >= 0))                 # likelihood should be non-negative
})