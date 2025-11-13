# test-optim_mll.R
library(testthat)
library(tibble)

# Simulación de datos ambientales [variables, tiempo, ubicación]
env_dat <- array(c(
  13, 13, 12, 2, 3, 3,
  15, 16, 15, 3, 5, 3,
  14, 14, 13, 2, 3, 3,
  13, 13, 12, 2, 3, 4
), dim = c(2, 3, 4))

# Vector de ocurrencias (presencia/ausencia por ubicación)
occ <- c(1, 0, 1, 0)

# Simulated auxiliar function to generate initial parameters
mock_start_param <- start_parms(env_dat = env_dat, num_starts = 100)

# Test returns
test_that("optim_mll returns expected output", {
  result <- optim_mll(
    env_dat = env_dat,
    occ = occ,
    parallel = FALSE,
    numstarts = 5
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("value", "convergence", "index") %in% colnames(result)))
  expect_equal(nrow(result), 5)
  expect_equal(result$index, 1:5)
})


test_that("optim_mll works with parallel = TRUE", {
  # Use the same simulated data
  env_dat <- array(c(
    13, 13, 12, 2, 3, 3,
    15, 16, 15, 3, 5, 3,
    14, 14, 13, 2, 3, 3,
    13, 13, 12, 2, 3, 4
  ), dim = c(2, 3, 4))

  occ <- c(1, 0, 1, 0)

  # Run with parallel = TRUE and fewer starts for speed
  nstarts <- 5
  result <- optim_mll(
    env_dat = env_dat,
    occ = occ,
    parallel = TRUE,
    numstarts = nstarts
  )

  # Check structure
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("value", "convergence", "index") %in% colnames(result)))

  # Check the number of parameters given the environment variables
  # (2 environment variables)
  expect_equal(ncol(result), (xsdmMle::numparms(p = dim(env_dat)[1]) + 3))

  # Check the index number
  expect_equal(result$index, 1:nstarts)

  # Check the number of rows
  expect_equal(nrow(result), nstarts)
})
