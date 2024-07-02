
setwd("your wd")

source("aux_functions.R")

library(rstan)
library(HDInterval)
library(truncnorm)
library(bayesplot)

### Load data
#############

load("data_linear_regression.RData")

### Load Stan files
###################

rstan_options(auto_write = TRUE)

lin_reg <- stan_model("linear_regression.stan")
lin_reg_prior <- stan_model("linear_regression_prior.stan")

### Prior Predictive checks
###########################

### Variance: sigma

# 1. Start with sigma ~ N+(0,1)

set.seed(123)

y_sim <- matrix(nrow=1000, ncol=200)
for (s in 1:1000) {
  
  std_dev <- rtruncnorm(1, 0, Inf, 1)
  y_sim[s, ] <- rnorm(200, 0, std_dev)
  
}

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 2. Try with with sigma ~ N+(0,2.5)

set.seed(123)

y_sim <- matrix(nrow=1000, ncol=200)
for (s in 1:1000) {
  
  std_dev <- rtruncnorm(1, 0, Inf, 2.5)
  y_sim[s, ] <- rnorm(200, 0, std_dev)
  
}

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 3. Try with with sigma ~ N+(0,5)

set.seed(123)

y_sim <- matrix(nrow=1000, ncol=200)
for (s in 1:1000) {
  
  std_dev <- rtruncnorm(1, 0, Inf, 5)
  y_sim[s, ] <- rnorm(200, 0, std_dev)
  
}

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Intercept: alpha

# 1. Start with alpha ~ N(0,5)

set.seed(123)

y_sim <- matrix(nrow=1000, ncol=200)
for (s in 1:1000) {

  std_dev <- rtruncnorm(1, 0, Inf, 2.5)
  mu <- rnorm(1, 0, 5)
  y_sim[s, ] <- rnorm(200, mu, std_dev)
  
}

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 2. Try with alpha ~ N(0,10)

set.seed(123)

y_sim <- matrix(nrow=1000, ncol=200)
for (s in 1:1000) {
  
  std_dev <- rtruncnorm(1, 0, Inf, 2.5)
  mu <- rnorm(1, 0, 10)
  y_sim[s, ] <- rnorm(200, mu, std_dev)
  
}

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Slopes: beta1 and beta2

# 1. Start with beta1=beta2 ~ N(0,5)

dat_list <- list(
  N = length(y),
  x1 = x1,
  x2 = x2,
  y = y,
  mu_b1 = 0,
  mu_b2 = 0,
  sigma_b1 = 5,
  sigma_b2 = 5,
  mu_alpha = 0,
  sigma_alpha = 10,
  lambda_sigma = 2.5
)

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

par(mfrow=c(1,3))
plot_prior_dens(y_sim, y)
plot_prior_lines(x1, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta1, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x1")
plot_prior_lines(x2, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta2, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x2")
par(mfrow=c(1,1))

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 3. Update prior on slopes to beta1=beta2 ~ N(0,2)

dat_list$sigma_b1 <- 2
dat_list$sigma_b2 <- 2

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

par(mfrow=c(1,3))
plot_prior_dens(y_sim, y)
plot_prior_lines(x1, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta1, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x1")
plot_prior_lines(x2, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta2, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x2")
par(mfrow=c(1,1))

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 3. update prior on sigma_alpha to alpha ~ N(0,2.5)

dat_list$sigma_alpha <- 2.5

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

par(mfrow=c(1,3))
plot_prior_dens(y_sim, y)
plot_prior_lines(x1, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta1, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x1")
plot_prior_lines(x2, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta2, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x2")
par(mfrow=c(1,1))

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# narrows the prior on the intercept (mean)

### Variables standardization (Default priors)

y_std <- as.vector(scale(y))
x1_std <- as.vector(scale(x1))
x2_std <- as.vector(scale(x2))

dat_list <- list(
  N = length(y_std),
  x1 = x1_std,
  x2 = x2_std,
  y = y_std,
  mu_b1 = 0,
  mu_b2 = 0,
  sigma_b1 = 1,
  sigma_b2 = 1,
  mu_alpha = 0,
  sigma_alpha = 0,
  lambda_sigma = 1
)

prior_sample <- sampling(lin_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)
y_sim <- prior_sample_fit$y_pred

par(mfrow=c(1,3))
plot_prior_dens(y_sim, y_std)
plot_prior_lines(x1, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta1, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x1")
plot_prior_lines(x2, 
                 prior_sample_fit$alpha, 
                 prior_sample_fit$beta2, 
                 y_sim,
                 ylim = c(-40, 40), xlab = "x2")
par(mfrow=c(1,1))

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(y_std),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(y_std),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(y_std),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Estimation
##############

fit <- sampling(lin_reg,
                data = dat_list,
                chains = 4,
                cores = 4)

sample_fit <- extract(fit)

### Transform coefficients back to the data scale

beta1 <- sample_fit$beta1 * (sd(y)/sd(x1))
beta2 <- sample_fit$beta2 * (sd(y)/sd(x2))
alpha <- mean(y)-(beta1*mean(x1))-(beta2*mean(x2))

### Visualize posteriors

par(mfrow=c(1,3), pty="s")
hist(alpha, main = "alpha", xlab = "draws")
hist(beta1, main = "beta1", xlab = "draws")
hist(beta2, main = "beta2", xlab = "draws")
par(mfrow=c(1,1), pty="m")

### Compute probabilities

thr <- 2
h_data <- hist(beta2, plot = F)
h_area <- cut(h_data$breaks, c(-Inf, thr, Inf))
prop <-  mean(beta2>thr)
  
par(pty="s")
plot(h_data, col = c("white", "lightblue")[h_area], main = "beta2", 
     xlab = paste0("Pr(beta2 > ", thr, " | y, x1, x2) = ", round(prop, 2)))
par(pty="m")

### Compute credible intervals

rbind(quantile(sample_fit$beta1, c(.025, .975)),
      quantile(sample_fit$beta2, c(.025, .975)))

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

### Diagnostic
##############

params <- c("beta1", "beta2", "sigma")

mcmc_trace(fit, pars = params)
mcmc_acf(fit, pars = params)
mcmc_pairs(fit, pars = params)

rhat(fit, pars = params)
neff_ratio(fit, pars = params)
