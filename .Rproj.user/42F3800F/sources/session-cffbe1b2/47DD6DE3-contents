#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

struct ColumnWorker : public Worker {
  const RMatrix<double> orthoM;
  const RMatrix<double> envM;
  const RMatrix<double> DL;
  const RMatrix<double> DRL;
  const RVector<double> mu;
  RVector<double> output;

  ColumnWorker(const NumericMatrix& orthoM,
               const NumericMatrix& envM,
               const NumericMatrix& DL,
               const NumericMatrix& DRL,
               NumericVector& mu,
               NumericVector& output
               )
    : orthoM(orthoM), envM(envM), DL(DL), DRL(DRL), mu(mu),
      output(output){}
  
  void operator()(std::size_t begin, std::size_t end) {
    int nrowOrthoM = orthoM.nrow();
    int ncolOrthoM = orthoM.ncol();
    
    for (std::size_t j = begin; j < end; j++) {
      double col_sum = 0.0;
      
      // For each column j in B, compute the dot product with each row of A
      for (int i = 0; i < nrowOrthoM; i++) {
        double dot_product = 0.0;
        for (int k = 0; k < ncolOrthoM; k++) {
          dot_product += orthoM(i, k) * (envM(k, j) - mu[k] );
        }
        //col_sum += dot_product;
        double usym = (DL(i, i)*dot_product + DRL(i, i)*std::max(0.0, dot_product));
        col_sum += usym * usym;
      }
      
      output[j] = col_sum;
      
    }
  }
};

// [[Rcpp::export(likeLtsg)]] 
NumericVector likeLtsg(NumericMatrix orthoM,
                   NumericMatrix envM,
                   NumericMatrix DL,
                   NumericMatrix DRL,
                   NumericVector mu,
                   int q,
                   int r) {
  if (orthoM.ncol() != envM.nrow()) {
    stop("Matrix dimensions are not compatible for multiplication.");
  }
  
  int ncolEnvM = envM.ncol();
  NumericVector output(ncolEnvM);
  
  ColumnWorker worker(orthoM, envM, DL, DRL, mu, output);
  parallelFor(0, ncolEnvM, worker);
  
  // Step 2: Reshape v into matrix D (q x r)
  NumericMatrix D(q, r);
  for (int j = 0; j < r; j++) {
    for (int i = 0; i < q; i++) {
      int idx = j * q + i;
      D(i, j) = output[idx]/(2*q);
    }
  }
  
  // Step 3: Sum columns of D to get final result (vector of r elements)
  NumericVector result(r);
  for (int j = 0; j < r; j++) {
    double sum = 0.0;
    for (int i = 0; i < q; i++) {
      sum += D(i, j);
    }
    result[j] = sum;
  }
  
  return result;
}