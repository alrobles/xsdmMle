#' Build an orthogonal matrix given a set of real parameters
#' @param entries A numeric vector of entries to build the orthogonal matrix.
#' @returns An orthogonal matrix of dimensions according to the given vector
#' @export
#'
#' @examples
#' build_o_matrix(0) # Identity
build_o_matrix <- function(entries) {
  checkmate::assert_numeric(entries, any.missing = FALSE, null.ok = TRUE)
  # get number of matrix entries given the parameter vector
  if (is.null(entries)) {
    matrix(1, 1, 1)
  } else {
    f <- function(n) 0.5 * (1 + sqrt(8 * n + 1))
    k <- f(length(entries))
    sk <- matrix(0, nrow = k, ncol = k)
    sk[lower.tri(sk)] <- entries
    sk <- sk - t(sk)
    o_mat <- expm::expm(sk)
    o_mat
  }
}
