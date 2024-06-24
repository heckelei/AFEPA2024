
data {
  
    int<lower=0> N; // number of observations
    vector[N] x1;   // covariate 1
    vector[N] x2;   // covariate 2
    real mu_b1;
    real mu_b2;
    real mu_alpha;
    real sigma_alpha;
    real lambda_b;

}

generated quantities {
  
    array[N] int<lower=0, upper=1> y_pred;
    vector[N] mu;
    vector[N] prob;
    real alpha;
    real beta1;
    real beta2;
    real<lower=0> sigma_b;
    
    alpha = normal_rng(mu_alpha, sigma_alpha);
    sigma_b = exponential_rng(lambda_b);
    beta1 = normal_rng(mu_b1, sigma_b);
    beta2 = normal_rng(mu_b2, sigma_b);
    
    for (i in 1:N) {
      
      mu[i] = alpha + beta1 * x1[i] + beta2 * x2[i];
      prob[i] = inv_logit(mu[i]);
      y_pred[i] = bernoulli_logit_rng(mu[i]);
      
    }
    
}
