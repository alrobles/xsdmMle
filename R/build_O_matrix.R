#' Build an orthogonal matrix given a set of real parameters
#' @param entries A vector of entries to build the orthogonal matrix.
#' @returns An orthogonal matrix of dimensions according to the given vector
#' @export
#'
#' @examples
#' build_O_matrix(0) #Identity
build_O_matrix <- function(entries){
  entries <- as.numeric(entries)
  #get number of matrix entries given the parameter vector
  if(is.null(entries) ){
    return(matrix(1, 1, 1))
  } else{
    f <- function(n) 0.5 * (1 + sqrt(8 * n + 1))
    k <- f(length(entries))
    
    sk <- matrix(0, nrow = k, ncol = k)
    sk[lower.tri(sk)] <- entries
    sk = sk - t(sk)
    O = expm::expm(sk)
    return(O)
  }
  
  
}

