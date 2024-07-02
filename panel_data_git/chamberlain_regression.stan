
data {
  
  int<lower=0> N; 
  int<lower=1> K;
  int<lower=1> J;
  int<lower=1> L;
  array[N] int<lower=1, upper=J> jj;
  matrix[N, K] X;
  matrix[J, L] U;
  vector[N] y;
  real<lower=0> sd_inter;
  real mu_inter;
  real<lower=0> sd_beta;
  real<lower=0> sd_sigma_y;
  real<lower=0> sd_sigma_g;
  real<lower=0> sd_sigma_a;
  
}

parameters {
  
  vector[J] alpha_raw;  
  vector[L] gamma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_g;

  vector[sd_inter>0] inter;
  vector[K] beta;
  real<lower=0> sigma_y;  
  
}

transformed parameters {
  
  vector[J] alpha;
  vector[J] mu_alpha;
  vector[N] mu;
  
  mu_alpha = U * gamma;
  alpha = mu_alpha + alpha_raw * sigma_a; 
  
  if (sd_inter == 0) {
    
    for(n in 1:N) mu[n] = X[n, ] * beta + alpha[jj[n]];
    
  } else {
    
    for(n in 1:N) mu[n] = inter[1] + X[n, ] * beta + alpha[jj[n]];
  }
    
}

model {
  
    if (sd_inter == 0) {
      
      alpha_raw ~ std_normal();
      gamma ~ normal(0, sigma_g);
      sigma_g ~ normal(0, sd_sigma_g); 
      sigma_a ~ normal(0, sd_sigma_a);
      
      beta ~ normal(0, sd_beta);
      sigma_y ~ normal(0, sd_sigma_y);
      
      y ~ normal(mu, sigma_y);
      
    } else {
      
      alpha_raw ~ std_normal();
      gamma ~ normal(0, sigma_g);
      sigma_g ~ normal(0, sd_sigma_g); 
      sigma_a ~ normal(0, sd_sigma_a);
      
      inter ~ normal(mu_inter, sd_inter);
      beta ~ normal(0, sd_beta);
      sigma_y ~ normal(0, sd_sigma_y);
      
      y ~ normal(mu, sigma_y);
      
    }
  
}

generated quantities {
  
  vector[N] y_rep;
  
  for (n in 1:N) y_rep[n] = normal_rng(mu[n], sigma_y);
  
}



