
data {
  
    int<lower=1> N; // number of observations
    int<lower=0> P; // number of covariates
    matrix[N, P] X; // covariates
    vector[N] y;    // dependent variable
    real mu_b;
    real sigma_b;
    real mu_alpha;
    real sigma_alpha;
    real lambda_sigma;
    
}

parameters {
  
    vector[sigma_alpha>0] alpha;  // intercept
    matrix[P, sigma_b>0] beta;     // coefficients
    real<lower=0> sigma; // standard deviation of the error term
    
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
      sigma ~ normal(0, lambda_sigma);

      y ~ normal(mu, sigma);
      
    } else if (sigma_alpha > 0 && sigma_b > 0) {
      
      alpha ~ normal(mu_alpha, sigma_alpha);
      to_vector(beta) ~ normal(mu_b, sigma_b);
      sigma ~ normal(0, lambda_sigma);

      y ~ normal(mu, sigma);
      
    }  else if (sigma_b == 0 && sigma_alpha>0) {
      
      alpha ~ normal(mu_alpha, sigma_alpha);
      sigma ~ normal(0, lambda_sigma);

      y ~ normal(mu, sigma);
      
    } else if (sigma_b == 0 && sigma_alpha==0) {
      
      sigma ~ normal(0, lambda_sigma);

      y ~ normal(0, sigma);
      
    }
}

generated quantities {
  
    vector[N] y_pred;
    
    for (i in 1:N) {
        y_pred[i] = normal_rng(mu[i], sigma);
    }
}
