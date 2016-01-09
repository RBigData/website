#----------------------------------------------------------------
# test function
#----------------------------------------------------------------

library(inline)

body <- "std::cout << \"It works\" << std::endl;"
test <- cxxfunction(signature(), body=body, plugin="Rcpp")

test()



#----------------------------------------------------------------
# Estimating pi
#----------------------------------------------------------------

mcsim_pi_r <- function(n){
  r <- 0L
  
  for (i in 1:n){
    u <- runif(1)
    v <- runif(1)
    
    if (u^2 + v^2 <= 1)
      r <- r + 1
  }
  
  return( 4*r/n )
}

mcsim_pi_r_vectorized <- function(n){
  x <- matrix(runif(n * 2), ncol=2)
  r <- sum(rowSums(x^2) <= 1)
  
  return( 4*r/n )
}

library(inline)
cxx_pi <- cxxfunction(signature(n_="int"), body='
    int i, r = 0;
    int n = Rcpp::as<int >(n_);
    double u, v;
    
    for (i=0; i<n; i++){
      u = R::runif(0, 1);
      v = R::runif(0, 1);
      
      if (u*u + v*v <= 1)
        r++;
    }
    
    return Rcpp::wrap( (double) 4.*r/n );
    
  ',plugin="Rcpp"
)

mcsim_pi_r_rcpp <- function(n){
  cxx_pi(as.integer(n))
}

library(rbenchmark)

n <- 50000

benchmark(R.loops = mcsim_pi_r(n), 
          R.vectorized = mcsim_pi_r_vectorized(n), 
          Rcpp = mcsim_pi_r_rcpp(n), 
          columns=c("test", "replications", "elapsed", "relative"))




#----------------------------------------------------------------
# Cosine similarity
#----------------------------------------------------------------

### cosine function from lsa package
cosine <- function (x, y = NULL){
   if (is.matrix(x) && is.null(y)) {
       co = array(0, c(ncol(x), ncol(x)))
       f = colnames(x)
       dimnames(co) = list(f, f)
       for (i in 2:ncol(x)) {
           for (j in 1:(i - 1)) {
               co[i, j] = cosine(x[, i], x[, j])
           }
       }
       co = co + t(co)
       diag(co) = 1
       return(as.matrix(co))
   }
   else if (is.vector(x) && is.vector(y)) {
       return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
   }
   else {
       stop("argument mismatch. Either one matrix or two vectors needed as input.")
   }
}



### Improved R solution
cosine2 <- function(x){
  cp <- crossprod(x)
  dg <- diag(cp)
  
  co <- matrix(0.0, length(dg), length(dg))
  
  for (j in 2L:length(dg)){
    for (i in 1L:(j-1L)){
      co[i, j] <- cp[i, j] / sqrt(dg[i] * dg[j])
    }
  }
  
  co <- co + t(co)
  diag(co) <- 1.0
  
  return( co )
}




### Rcpp
library(inline)

fill_loop <- cxxfunction(
  signature(cp_="matrix", dg_="numeric"),
  body='
    // Shallow copies
    Rcpp::NumericMatrix cp(cp_);
    Rcpp::NumericVector dg(dg_);
    
    // Allocate return
    Rcpp::NumericMatrix co(cp.nrow(), cp.ncol());
    
    int i, j;
    
    for (j=0; j<co.ncol(); j++){
      for (i=0; i<co.nrow(); i++){
        if (i == j)
          co(i, j) = 1.0;
        else
          co(i, j) = cp(i, j) / std::sqrt(dg[i] * dg[j]);
      }
    }
    
    return co;
  ',plugin="Rcpp"
)


cosine_Rcpp <- function(x){
  cp <- crossprod(x)
  dg <- diag(cp)
  
  co <- fill_loop(cp, dg)
  
  return( co )
}



### Rcpp improved
fill_loop2 <- cxxfunction(
  signature(cp_="matrix", dg_="numeric"),
  body='
    
    // Shallow copies
    Rcpp::NumericMatrix cp(cp_);
    Rcpp::NumericVector dg(dg_);
    
    const unsigned int n = cp.nrow();
    
    // Allocate return
    Rcpp::NumericMatrix co(n, n);
    
    int i, j;
    
    // Fill diagonal
    for (j=0; j<n; j++)
      co(j, j) = 1.0;
    
    // Fill lower triangle
    for (j=0; j<n; j++){
      for (i=0; i<j; i++)
        co(i, j) = cp(i, j) / std::sqrt(dg[i] * dg[j]);
    }
    
    // Copy lower triangle to upper
    for (j=0; j<n; j++){
      for (i=j+1; i<n; i++)
        co(i, j) = co(j, i);
    }
    
    return co;
  ',plugin="Rcpp"
)

cosine_Rcpp2 <- function(x){
  cp <- crossprod(x)
  dg <- diag(cp)
  
  co <- fill_loop2(cp, dg)
  
  return( co )
}



#----------------------------------------------------------------
# RcppArmadillo
#----------------------------------------------------------------

f <- function(x) list(outer=x %*% t(x), inner=t(x) %*% x)


body <- '
  arma::mat v = Rcpp::as<arma::mat>(vs);
  arma::mat op = v * v.t();
  arma::mat ip = v.t()*v;
  
  return Rcpp::List::create(
    Rcpp::Named("outer")=op, Rcpp::Named("inner") = ip);
'

library(inline)
g <- cxxfunction(signature(vs="matrix"), plugin="RcppArmadillo", body=body)



x <- matrix(1:30, 10)
all.equal(f(x), g(x))



#----------------------------------------------------------------
# RcppGSL
#----------------------------------------------------------------

includes <- '
  #include <gsl/gsl_matrix.h>
  #include <gsl/gsl_blas.h>
'

body <- '
  RcppGSL::matrix<double> M = sM;
  int k = M.ncol();
  Rcpp::NumericVector n(k);
  
  for (int j = 0; j < k; j++) {
      RcppGSL::vector_view<double> colview = gsl_matrix_column(M, j);
      n[j] = gsl_blas_dnrm2(colview);
  }
  
  M.free() ;
  return n;
'

library(inline)
g <- cxxfunction(signature(sM="matrix"), plugin="RcppGSL", body=body, inc=includes)


