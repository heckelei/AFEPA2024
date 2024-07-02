
functions {

real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb, mu, sigma);
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;
    }
  
}

data {
  
  int<lower=0> N; 
  int<lower=1> K;
  int<lower=1> J;
  int<lower=1> L;
  array[N] int<lower=1, upper=J> jj;
  matrix[N, K] X;
  matrix[J, L] U;
  real<lower=0> sd_inter;
  real mu_inter;
  real<lower=0> sd_sigma_y;

}

generated quantities {
  
  vector[N] y;
  real inter;
  real<lower=0> sigma_y;
  
  if (sd_inter == 0) {
    
    inter = 0;
    
  } else {
    
    inter = normal_rng(mu_inter, sd_inter);
    
  }

  sigma_y = normal_lb_rng(0, sd_sigma_y, 0);
  
  for(n in 1:N) {
    
    y[n] = normal_rng(inter, sigma_y);
    
  }

}
