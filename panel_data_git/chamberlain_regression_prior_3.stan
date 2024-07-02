
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
  real<lower=0> sd_sigma_y;
  real<lower=0> sd_sigma_g;
  real<lower=0> sd_sigma_a;
  
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
  real<lower=0> sigma_y; 
  
  sigma_g = normal_lb_rng(0, sd_sigma_g, 0);
  for (l in 1:L) gamma[l] = 0 + normal_rng(0, 1) * sigma_g;
  
  mu_alpha = U * gamma;
  
  sigma_alpha = normal_lb_rng(0, sd_sigma_a, 0);
  for(j in 1:J) alpha[j] = mu_alpha[j] + normal_rng(0, 1) * sigma_alpha;
  
  if (sd_inter==0) {
    
    inter = 0;
    
  } else {
    
    inter = normal_rng(0, sd_inter);
    
  }
  
  sigma_y = normal_lb_rng(0, sd_sigma_y, 0);
  
  for(n in 1:N) {
    
    mu[n] = inter + alpha[jj[n]];
    y[n] = normal_rng(mu[n], sigma_y);
    
  }

}
