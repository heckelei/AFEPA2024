
data {
  
    int<lower=1> N; // number of observations
    int<lower=0> P; // number of covariates
    matrix[N, P] X;   // covariates
    real mu_b;
    real sigma_b;
    real mu_alpha;
    real sigma_alpha;

}

generated quantities {
  
    array[N] int<lower=0, upper=1> y_pred;
    vector[N] mu;
    vector[N] prob;
    real alpha;
    vector[P] beta;

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

    for (i in 1:N) {
      
      mu[i] = alpha + X[i,] * beta;
      prob[i] = inv_logit(mu[i]);
      y_pred[i] = bernoulli_logit_rng(mu[i]);
      
    }
    
}
