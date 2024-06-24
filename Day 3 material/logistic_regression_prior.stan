
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

}

generated quantities {
  
    array[N] int<lower=0, upper=1> y_pred;
    vector[N] mu;
    vector[N] prob;
    real alpha;
    real beta1;
    real beta2;
    
    alpha = normal_rng(mu_alpha, sigma_alpha);
    beta1 = normal_rng(mu_b1, sigma_b1);
    beta2 = normal_rng(mu_b2, sigma_b2);
    
    for (i in 1:N) {
      
      mu[i] = alpha + beta1 * x1[i] + beta2 * x2[i];
      prob[i] = inv_logit(mu[i]);
      y_pred[i] = bernoulli_logit_rng(mu[i]);
      
    }
    
}
