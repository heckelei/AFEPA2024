
setwd("/Users/alessandro.varacca/Library/CloudStorage/OneDrive-UniversitàCattolicadelSacroCuore/1_Lavoro_personale/2024/AFEPA_Summer_School")

source("aux_functions.R")

library(rstan)
library(truncnorm)

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

library(dplyr)

# put dependent variable into a vector called y

y <- dat_reg_logit$Net_Farm_Income

# put matrix of independent variables into a matrix called X. This must be a matrix,
# NOT a data.frame or tibble object!
  
X <- dat_reg_logit %>%
  select(-Net_Farm_Income) %>%
  as.matrix()
  
### Prior Predictive checks
###########################

### Standardized variables

y_std <- as.vector(scale(y))
X_std <- apply(X, 2, scale)

dat_list_std <- list(
  N = length(y),
  P = ncol(X),
  X = X_std,
  y = y_std,
  mu_b = 0,
  sigma_b = 1,
  mu_alpha = 0,
  sigma_alpha = 0,
  lambda_sigma = 1
)

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list_std,
                         algorithm = "Fixed_param")

prior_sample_fit <- rstan::extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

max_y <- apply(y_sim, 1, max)
min_y <- apply(y_sim, 1, min)
sd_y  <- apply(y_sim, 1, sd)

par(mfrow=c(1,3), pty="s")
hist(max_y, main = "max", xlab = "draws")
abline(v=max(y_std),col="red", lwd=2)
hist(min_y, main = "min", xlab = "draws")
abline(v=min(y_std),col="blue", lwd=2)
hist(sd_y, main = "sd", xlab = "draws")
abline(v=sd(y_std),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# Notice how the prior predictive distribution generate values that are too
# extreme and has a very large variance. This is typically the case when our model
# includes many variables.

### Decrease lambda and beta

dat_list_std$lambda_sigma <- .5
dat_list_std$sigma_b <- .5

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list_std,
                         algorithm = "Fixed_param")

prior_sample_fit <- rstan::extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

max_y <- apply(y_sim, 1, max)
min_y <- apply(y_sim, 1, min)
sd_y  <- apply(y_sim, 1, sd)

par(mfrow=c(1,3), pty="s")
hist(max_y, main = "max", xlab = "draws")
abline(v=max(y_std),col="red", lwd=2)
hist(min_y, main = "min", xlab = "draws")
abline(v=min(y_std),col="blue", lwd=2)
hist(sd_y, main = "sd", xlab = "draws")
abline(v=sd(y_std),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Estimation
##############

fit <- sampling(lin_reg,
                data = dat_list_std,
                chains = 4,
                cores = 4)

sample_fit <- rstan::extract(fit)

beta <- sample_fit$beta[,,1]

### Un-standardize variables

norm_factor <- sd(y)/apply(X, 2, sd)
beta_unst <- sweep(beta, 2, norm_factor, FUN = '*')
colnames(beta_unst) <- colnames(X)

# Summary statistics of the marginal posterior for the slope parameters

summary(beta_unst)

# Check it against the result form a standard linear regression model

summary(lm(y ~ X))

### Visualize posterior distribution of b
# for convenience, we transform the matrix into a data.frame and use 
# ggplot2

library(tidyr)
library(ggplot2)

data.frame(beta_unst) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "post_draw") %>%
  group_by(variable) %>%
  mutate(m = mean(post_draw)) %>%
  ggplot(aes(x = post_draw)) +
  geom_histogram() +
  geom_vline(aes(xintercept = m), col = "red", lty = 2) +
  facet_wrap(. ~ variable, scales = "free") +
  theme_bw()

### for each slope, calculate the probability that the parameter is greater
# than zero

apply(beta_unst, 2, function(x) round(mean(x>0),3)*100)

### Calculate Credible intervals

apply(beta_unst, 2, quantile, c(.025, .975))

### Posterior Predictive Distribution

y_pred <- sample_fit$y_pred

plot_prior_dens(y_pred, y_std, main = "PPD")

par(mfrow=c(1,3), pty="s")
hist(apply(y_pred, 1, max), main = "max", xlab = "draws")
abline(v=max(y_std),col="red", lwd=2)
hist(apply(y_pred, 1, min), main = "min", xlab = "draws")
abline(v=min(y_std),col="blue", lwd=2)
hist(apply(y_pred, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y_std),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")
  
### Let us see what happens when we distinguish between farms with negative NFI
# and farms with positive NFI

# The simplest approach is to create a dummy variable d=1 when NFI < 0 and multiply
# all the other covariates by d:

X_new_std <- dat_reg_logit %>%
  mutate(d = Net_Farm_Income<0) %>%
  mutate(across(-d, ~.*d, .names = "{.col}_d")) %>%
  select(-Net_Farm_Income) %>%
  mutate_all(scale) %>%
  as.matrix()

# We now re-define the data list and re-estimate the model:

dat_list_std_new <- list(
  N = length(y),
  P = ncol(X_new_std),
  X = X_new_std,
  y = y_std,
  mu_b = 0,
  sigma_b = .5,
  mu_alpha = 0,
  sigma_alpha = 0,
  lambda_sigma = .5
)

fit_new <- sampling(lin_reg,
                    data = dat_list_std_new,
                    chains = 4,
                    cores = 4)

sample_fit_new <- rstan::extract(fit_new)

y_pred_new <- sample_fit_new$y_pred

par(mfrow=c(1,3), pty="s")
hist(apply(y_pred_new, 1, max), main = "max", xlab = "draws")
abline(v=max(y_std),col="red", lwd=2)
hist(apply(y_pred_new, 1, min), main = "min", xlab = "draws")
abline(v=min(y_std),col="blue", lwd=2)
hist(apply(y_pred_new, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y_std),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# Much better now!
