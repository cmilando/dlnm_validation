data {
  int<lower=0> maxlag;
  int<lower=0> Xdim;
  int<lower=0> Vdim;
  matrix[Xdim, maxlag+1] logRRmat;
  matrix[Xdim*(maxlag+1), Vdim] XPredAll;
}

parameters {
  matrix[Vdim, 1] beta;
  real<lower=0> sigma;
}

model {
  for(vi in 1:(maxlag+1)) {
    
     int start_idx = (Xdim * (vi - 1)) + 1;
     int end_idx = Xdim * vi;
     
     target += normal_lpdf(logRRmat[, vi] | 
               to_row_vector(XPredAll[start_idx:end_idx, ] * beta), 
               sigma);
  }
}

