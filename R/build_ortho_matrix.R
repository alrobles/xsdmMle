#' Build an orthogonal matrix
#'
#' Build an orthogonal matrix given a set of real values.
#' 
#' The orthogonal matrix is constructed based on ...
#' @param entries A vector of entries to build the orthogonal matrix.
#' @returns An orthogonal matrix of dimensions according to the given vector
#' @export
#'
#' @examples
#' build_ortho_matrix(0) #Identity
build_ortho_matrix <- function(entries){
  
  entries <- as.numeric(entries)

  # calculate matrix dimensions
  if(is.null(entries) ){

    # return a 1x1 matrix with element = 1
    return(matrix(1, 1, 1))

  } else{

    # calculate ....
    f <- function(n) 0.5 * (1 + sqrt(8 * n + 1))
    k <- f(length(entries)) # this can be a non-integer, but used for matrix dimensions?
    
    # ...?
    sk <- matrix(0, nrow = k, ncol = k)
    sk[lower.tri(sk)] <- entries
    sk = sk - t(sk)
    ortho_matrix = expm::expm(sk)
    return(ortho_matrix)
  }  
  
}

