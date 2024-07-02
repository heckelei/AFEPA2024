
functions {
  
  real normal_lub_rng(real mu, real sigma, real lb) {
    
    real p_lb = normal_cdf(lb, mu, sigma);
    real u = uniform_rng(p_lb, 1);
    real y = mu + sigma * Phi(u);
    return y;
    
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
  real<lower=0> sd_sigma_y;
  real<lower=0> sd_sigma_g;
  real<lower=0> sd_sigma_a;
  real<lower=0> sd_beta;
  
}

generated quantities {
  
  vector[J] alpha;
  vector[J] mu_alpha;
  real<lower=0> sigma_g;
  vector[L] gamma;
  real<lower=0> sigma_alpha;

  vector[N] y;
  vector[N] mu;
  real inter;
  vector[K] beta;
  real<lower=0> sigma_y; 
  
  sigma_g = normal_lub_rng(0, sd_sigma_g, 0);
  for (l in 1:L) gamma[l] = 0 + normal_rng(0, 1) * sigma_g;
  
  mu_alpha = U * gamma;
  
  sigma_alpha = normal_lub_rng(0, sd_sigma_a, 0);
  for(j in 1:J) alpha[j] = mu_alpha[j] + normal_rng(0, 1) * sigma_alpha;
  
  if (sd_inter == 0) {
    
    inter = 0;
    
  } else {
    
    inter = normal_rng(0, sd_inter);
    
  }
  
  for(k in 1:K) beta[k] = normal_rng(0, sd_beta);
  sigma_y = normal_lub_rng(0, sd_sigma_y, 0);
  
  for(n in 1:N) {
    
    mu[n] = inter + X[n,]*beta + alpha[jj[n]];
    y[n] = mu[n] + normal_rng(0, 1) * sigma_y;
    
  }

}
