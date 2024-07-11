
functions {
  
real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb, mu, sigma);
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;
    }
  
}

data {
  
    int<lower=1> N; // number of observations
    int<lower=0> P; // number of covariates
    matrix[N, P] X;
    real mu_b;
    real sigma_b;
    real mu_alpha;
    real sigma_alpha;
    real lambda_sigma;

}

generated quantities {
  
    vector[N] y_pred;
    vector[N] mu;
    real alpha;
    vector[P] beta;
    real<lower=0> sigma;
    
    if (sigma_alpha==0) {
      
      alpha = 0;
      
    } else {
      
      alpha = normal_rng(mu_alpha, sigma_alpha);
      
    }
    
    if (sigma_b == 0) {
      
      for(p in 1:P) beta[p] = 0;
      
    } else {
      
      for(p in 1:P) beta[p] = normal_rng(mu_b, sigma_b);
      
    }
    
    sigma = normal_lb_rng(0, lambda_sigma, 0);
    
    for (i in 1:N) {
      
      mu[i] = alpha + X[i,] * beta;
      y_pred[i] = mu[i] + normal_rng(0, 1) * sigma;
      
    }
    
}
