library(testthat)

test_that("log1pexp computes correct values in different cutoff regions", {
  # Values in region c0 < x <= c1 (should use log1p(exp(x)))
  x_mid <- c(-10, 0, 10)
  result_mid <- log1pexp(x_mid)
  expected_mid <- log1p(exp(x_mid))
  expect_equal(result_mid, expected_mid, tolerance = 1e-12)
  
  # Values in region x > c2 (should approximate as x)
  x_large <- c(40, 50)
  result_large <- log1pexp(x_large)
  expect_equal(result_large, x_large, tolerance = 1e-12)
  
  # Values in region c1 < x <= c2 (should use x + 1/exp(x))
  x_transition <- c(20, 30)
  result_transition <- log1pexp(x_transition)
  expected_transition <- x_transition + 1 / exp(x_transition)
  expect_equal(result_transition, expected_transition, tolerance = 1e-12)
})

test_that("log1pexp handles NA values correctly", {
  x <- c(-10, NA, 10)
  result <- log1pexp(x)
  expect_true(is.na(result[2]))
  expect_equal(result[1], log1p(exp(-10)), tolerance = 1e-12)
  expect_equal(result[3], log1p(exp(10)), tolerance = 1e-12)
})

test_that("log1pexp respects custom cutoffs", {
  x <- c(0, 20, 40)
  # Custom cutoffs: force first value into log1p region, second into transition, third into large
  result <- log1pexp(x, c0 = -5, c1 = 15, c2 = 35)
  expect_equal(result[1], log1p(exp(0)), tolerance = 1e-12)
  expect_equal(result[2], 20 + 1 / exp(20), tolerance = 1e-12)
  expect_equal(result[3], 40, tolerance = 1e-12)
})

test_that("log1pexp handles extreme negative values without warnings", {
  x <- c(-100, -50)
  # Suppress warnings for value check (if any occur internally)
  result <- suppressWarnings(log1pexp(x))
  expected <- exp(x) # For very negative x, log1p(exp(x)) ~ exp(x)
  expect_equal(result, expected, tolerance = 1e-12)
})