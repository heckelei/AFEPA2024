
functions {
  
real normal_lb_rng(real mu, real sigma, real lb) {
    real p = normal_cdf(lb, mu, sigma);
    real u = uniform_rng(p, 1);
    return (sigma * inv_Phi(u)) + mu;
    }
  
}

data {
  
    int<lower=0> N; // number of observations
    vector[N] x1;   // covariate 1
    vector[N] x2;   // covariate 2
    real mu_b1;
    real mu_b2;
    real sigma_b1;
    real sigma_b2;
    real mu_alpha;
    real sigma_alpha;
    real lambda_sigma;

}

generated quantities {
  
    vector[N] y_pred;
    vector[N] mu;
    real alpha;
    real beta1;
    real beta2;
    real<lower=0> sigma;
    
    if (sigma_alpha==0) {
      
      alpha = 0;
      
    } else {
      
      alpha = normal_rng(mu_alpha, sigma_alpha);
      
    }
    
    beta1 = normal_rng(mu_b1, sigma_b1);
    beta2 = normal_rng(mu_b2, sigma_b2);
    sigma = normal_lb_rng(0, lambda_sigma, 0);
    
    for (i in 1:N) {
      
      mu[i] = alpha + beta1 * x1[i] + beta2 * x2[i];
      y_pred[i] = mu[i] + normal_rng(0, 1) * sigma;
      
    }
    
}
