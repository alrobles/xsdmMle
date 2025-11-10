#' This is used to calculated horizontal distance
#' between parameters
#'
#' @param bparms  A list with parameters to adjust to a canonical form
#' @returns A list  with parameters in canonical form
#'
#' @export
#'
#' @examples
#'
#' paramTableExample[1, ] |>
#'   math_to_bio() |>
#'   make_cannonical()
make_cannonical <- function(bparms) {
  # extract the components that need to be changed
  o_mat <- bparms$o_mat
  sigl <- bparms$sigl
  sigr <- bparms$sigr

  # first multiply each column of O by 1 or -1 to make the top-most
  # non-zero entry be positive
  for (cc in 1:(dim(o_mat)[2])) {
    h <- o_mat[, cc]
    firstnzind <- min(which(h != 0))
    if (h[firstnzind] < 0) {
      o_mat[, cc] <- -h
      h <- sigl[cc]
      # switch the corresponding entries of sigl and sigr to pay the price
      sigl[cc] <- sigr[cc]
      sigr[cc] <- h
    }
  }

  # now reorder the columns of O using dictionary order, and bring the entries
  # of sigl and sigr "along for the ride"
  inds <- do.call(order, as.data.frame(t(o_mat)))
  o_mat <- o_mat[, inds]
  sigl <- sigl[inds]
  sigr <- sigr[inds]

  # reinsert the three altered components and return
  bparms$o_mat <- o_mat
  bparms$sigl <- sigl
  bparms$sigr <- sigr
  bparms
}
