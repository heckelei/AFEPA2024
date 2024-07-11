
data {
  
    int<lower=1> N; // number of observations
    int<lower=0> P; // number of covariates
    matrix[N, P] X; // covariates
    array[N] int<lower=0, upper=1> y;    // dependent variable
    real mu_b;
    real sigma_b;
    real mu_alpha;
    real sigma_alpha;
    
}

parameters {
  
    vector[sigma_alpha>0] alpha;  // intercept
    matrix[P, sigma_b>0] beta;    // coefficients

}

transformed parameters {
  
  vector[N] mu;
  
  if (sigma_alpha == 0 && sigma_b > 0) {
    
    mu = X * beta[,1];
    
  } else if (sigma_alpha > 0 && sigma_b > 0) {
    
    mu = alpha[1] + X * beta[,1];
    
  } else if (sigma_b == 0 && sigma_alpha>0) {
    
    mu = rep_vector(alpha[1], N);
    
  } else if (sigma_b == 0 && sigma_alpha == 0) {
    
    mu = rep_vector(0, N);
    
  }
  
}

model {
  
    if (sigma_alpha == 0 && sigma_b > 0) {
      
      to_vector(beta) ~ normal(mu_b, sigma_b);

      y ~ bernoulli_logit(mu);
      
    } else if (sigma_alpha > 0 && sigma_b > 0) {
      
      alpha ~ normal(mu_alpha, sigma_alpha);
      to_vector(beta) ~ normal(mu_b, sigma_b);

      y ~ bernoulli_logit(mu);
      
    }  else if (sigma_b == 0 && sigma_alpha>0) {
      
      alpha ~ normal(mu_alpha, sigma_alpha);

      y ~ bernoulli_logit(mu);
      
    } else if (sigma_b == 0 && sigma_alpha==0) {
      
      y ~ bernoulli_logit(mu);
      
    }
    
}

generated quantities {
  
    vector[N] prob;
    array[N] int<lower=0, upper=1> y_pred;
    
    for (i in 1:N) {
        prob[i] = inv_logit(mu[i]);
        y_pred[i] = bernoulli_logit_rng(mu[i]);
    }
}
