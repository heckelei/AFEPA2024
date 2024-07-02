
data {
  
    int<lower=0> N; // number of observations
    vector[N] x1;   // covariate 1
    vector[N] x2;   // covariate 2
    vector[N] y;    // dependent variable
    real mu_b1;
    real mu_b2;
    real sigma_b1;
    real sigma_b2;
    real mu_alpha;
    real sigma_alpha;
    real lambda_sigma;
    
}

parameters {
  
    vector[sigma_alpha>0] alpha;  // intercept
    real beta1;     // coefficient for covariate 1
    real beta2;     // coefficient for covariate 2
    real<lower=0> sigma; // standard deviation of the error term
    
}

transformed parameters {
  
  vector[N] mu;
  
  if (sigma_alpha == 0) {
    
    mu = beta1 * x1 + beta2 * x2;
    
  } else {
    
    mu = alpha[1] + beta1 * x1 + beta2 * x2;
    
  }
  
}

model {
  
    // Priors
    
    if (sigma_alpha == 0) {
      
      beta1 ~ normal(mu_b1, sigma_b1);
      beta2 ~ normal(mu_b2, sigma_b2);
      sigma ~ normal(0, lambda_sigma);

      // Likelihood
      y ~ normal(mu, sigma);
      
    } else {
      
      alpha ~ normal(mu_alpha, sigma_alpha);
      beta1 ~ normal(mu_b1, sigma_b1);
      beta2 ~ normal(mu_b2, sigma_b2);
      sigma ~ normal(0, lambda_sigma);

      // Likelihood
      y ~ normal(mu, sigma);
      
    }
    
}

generated quantities {
  
    vector[N] y_pred; // predicted values for y
    
    for (i in 1:N) {
        y_pred[i] = normal_rng(mu[i], sigma);
    }
}
