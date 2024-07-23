
setwd("your wd")

source("aux_functions.R")

library(rstan)
library(truncnorm)
library(dplyr)

### Load data
#############

load("dat_reg_logit.RData")

### Load the Stan files
#######################

rstan_options(auto_write = TRUE)

lin_reg <- stan_model("linear_regression_X.stan")
lin_reg_prior <- stan_model("linear_regression_prior_X.stan")

### Prepare the data
####################

# put dependent variable into a vector called y

y <- dat_reg_logit$Net_Farm_Income

# put matrix of independent variables into a matrix called X. This must be a matrix,
# NOT a data.frame or tibble object!
  
X <- dat_reg_logit %>%
  select(-Net_Farm_Income) %>%
  as.matrix()
  
### Prior Predictive checks
###########################

### Unstandardized variables

dat_list <- list(
  N = length(y),
  P = ncol(X),
  X = X,
  y = y,
  mu_b = 0,
  sigma_b = 0,
  mu_alpha = 0,
  sigma_alpha = 0,
  lambda_sigma = 5
)

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

max_y <- apply(y_sim, 1, max)
min_y <- apply(y_sim, 1, min)
sd_y  <- apply(y_sim, 1, sd)

par(mfrow=c(1,3), pty="s")
hist(max_y, main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(min_y, main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(sd_y, main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Standardized variables

y_std <- as.vector(scale(y))
X_std <- apply(X, 2, scale)

### Estimation
##############

fit <- sampling(lin_reg,
                data = dat_list,
                chains = 4,
                cores = 4)
