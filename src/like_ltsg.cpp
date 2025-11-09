#include <Rcpp.h>
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;

struct ColumnWorker : public Worker {
  const RMatrix<double> ortho_m;
  const RMatrix<double> env_m;
  const RMatrix<double> dl_mat;
  const RMatrix<double> drl_mat;
  const RVector<double> mu;
  RVector<double> output;

  ColumnWorker(const NumericMatrix& ortho_m,
               const NumericMatrix& env_m,
               const NumericMatrix& dl_mat,
               const NumericMatrix& drl_mat,
               NumericVector& mu,
               NumericVector& output
               )
    : ortho_m(ortho_m), env_m(env_m), dl_mat(dl_mat), drl_mat(drl_mat), mu(mu),
      output(output){}
  
  void operator()(std::size_t begin, std::size_t end) {
    int nrow_ortho_m = ortho_m.nrow();
    int ncol_ortho_m = ortho_m.ncol();
    
    for (std::size_t j = begin; j < end; j++) {
      double col_sum = 0.0;
      
      // For each column j in B, compute the dot product with each row of A
      for (int i = 0; i < nrow_ortho_m; i++) {
        double dot_product = 0.0;
        for (int k = 0; k < ncol_ortho_m; k++) {
          dot_product += ortho_m(i, k) * (env_m(k, j) - mu[k] );
        }
        //col_sum += dot_product;
        double usym = (dl_mat(i, i)*dot_product + drl_mat(i, i)*std::max(0.0, dot_product));
        col_sum += usym * usym;
      }
      
      output[j] = col_sum;
      
    }
  }
};

// [[Rcpp::export(like_ltsg)]] 
NumericVector like_ltsg(NumericVector mu,  
                   NumericMatrix env_m,
                   NumericMatrix dl_mat,
                   NumericMatrix drl_mat,
                   NumericMatrix ortho_m,
                   int q,
                   int r) {
  if (ortho_m.ncol() != env_m.nrow()) {
    stop("Matrix dimensions are not compatible for multiplication.");
  }
  
  int ncol_env_m = env_m.ncol();
  NumericVector output(ncol_env_m);
  
  ColumnWorker worker(ortho_m, env_m, dl_mat, drl_mat, mu, output);
  parallelFor(0, ncol_env_m, worker);
  
  // Step 2: Reshape v into matrix D (q x r)
  NumericMatrix d_mat(q, r);
  for (int j = 0; j < r; j++) {
    for (int i = 0; i < q; i++) {
      int idx = j * q + i;
      d_mat(i, j) = output[idx]/(2*q);
    }
  }
  
  // Step 3: Sum columns of D to get final result (vector of r elements)
  NumericVector result(r);
  for (int j = 0; j < r; j++) {
    double sum = 0.0;
    for (int i = 0; i < q; i++) {
      sum += d_mat(i, j);
    }
    result[j] = sum;
  }
  
  return result;
}