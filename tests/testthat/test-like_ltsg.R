library(testthat)

test_that("like_ltsg throws error for incompatible matrix dimensions", {
  # Create small matrices with incompatible dimensions
  mu <- c(1, 2)
  ortho_m <- matrix(1:4, nrow = 2, ncol = 2)   # 2x2
  env_m <- matrix(1:6, nrow = 3, ncol = 2)     # 3x2 (incompatible: ortho_m.ncol != env_m.nrow)
  dl_mat <- diag(2)
  drl_mat <- diag(2)
  
  # Expect an error with the specific message
  expect_error(
    like_ltsg(mu, env_m, dl_mat, drl_mat, ortho_m, q = 1, r = 2),
    "Matrix dimensions are not compatible for multiplication"
  )
})