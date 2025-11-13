library(testthat)

test_that("make_canonical correctly flips and reorders columns", {
  # Example input
  bparms <- list(
    o_mat = matrix(c(
      0, -2, 3,
      1,  0, 4,
      0,  0, 5
    ), nrow = 3, byrow = FALSE),
    sigl = c("L1", "L2", "L3"),
    sigr = c("R1", "R2", "R3")
  )
  
  # Apply function
  result <- make_canonical(bparms)
  
  # Check structure
  expect_true(is.list(result))
  expect_true(all(c("o_mat", "sigl", "sigr") %in% names(result)))
  
  # Check dimensions preserved
  expect_equal(dim(result$o_mat), dim(bparms$o_mat))
  
  # Identify which original column was flipped
  flipped_col <- 2 # original second column had negative top entry
  expect_true(all(result$sigl[result$o_mat[1, ] == 2] == "L2") ||
                all(result$sigr[result$o_mat[1, ] == 2] == "R2"))
  
  # Check that sigl and sigr for flipped column are swapped
  # Original sigl[2] was L2 and sigr[2] was R2
  # After flipping, they should be swapped
  flipped_index <- which(result$sigl == "L2" | result$sigr == "L2")
  expect_true(result$sigl[flipped_index] == "L2" || result$sigr[flipped_index] == "L2")
  
  # Check columns are ordered lexicographically
  ordered_inds <- do.call(order, as.data.frame(t(result$o_mat)))
  expect_equal(ordered_inds, seq_along(ordered_inds))
})