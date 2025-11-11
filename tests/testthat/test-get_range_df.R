library(testthat)

test_that("get_range_df returns correct structure and values", {
  # Create a small environmental array: p = 2 variables, 3 time steps, 2 locations
  set.seed(123)
  env_dat <- array(runif(2 * 3 * 2, min = 0.1, max = 1), dim = c(2, 3, 2))
  
  # Call the function
  result <- get_range_df(env_dat)
  
  # Check class and dimensions
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)  # lower, center, upper
  expect_true(all(c("lower", "center", "upper") %in% colnames(result)))
  
  # Check row names include mu, sigl, sigr, ctil, pd, o_par
  rn <- rownames(result)
  expect_true(any(grepl("^mu", rn)))
  expect_true(any(grepl("^sigl", rn)))
  expect_true(any(grepl("^sigr", rn)))
  expect_true("ctil" %in% rn)
  expect_true("pd" %in% rn)
  expect_true(any(grepl("^o_par", rn)))
  
  # Check that no NA remains in rows for mu, sigl, sigr
  mu_rows <- grep("^mu", rn)
  expect_false(any(is.na(result[mu_rows, ])))
  
  # Check quantile logic for mu rows
  mu_values <- as.numeric(env_dat[1, , ])
  expected_quantiles <- unname(stats::quantile(mu_values, probs = c(0.1, 0.5, 0.9)))
  expect_equal(as.numeric(result[mu_rows[1], ]), expected_quantiles, tolerance = 1e-8)
})