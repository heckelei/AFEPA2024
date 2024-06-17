
data {
  
    int<lower=0> N; // number of observations
    vector[N] x1;   // covariate 1
    vector[N] x2;   // covariate 2
    array[N] int<lower=0, upper=1> y;    // dependent variable
    real mu_b1;
    real mu_b2;
    real mu_alpha;
    real sigma_alpha;
    real lambda_b;
    
}

parameters {
  
    real alpha;     // intercept
    real beta1;     // coefficient for covariate 1
    real beta2;     // coefficient for covariate 2
    real<lower=0> sigma_b;

}

transformed parameters {
  
  vector[N] mu;
  
  mu = alpha + beta1 * x1 + beta2 * x2;
  
}

model {
  
    // Priors
    alpha ~ normal(mu_alpha, sigma_alpha);
    beta1 ~ normal(mu_b1, sigma_b);
    beta2 ~ normal(mu_b2, sigma_b);
    sigma_b ~ exponential(lambda_b);

    // Likelihood
    y ~ bernoulli_logit(mu);
    
}

generated quantities {
  
    vector[N] prob;
    array[N] int<lower=0, upper=1> y_pred;
    
    for (i in 1:N) {
        prob[i] = inv_logit(mu[i]);
        y_pred[i] = bernoulli_logit_rng(mu[i]);
    }
}
