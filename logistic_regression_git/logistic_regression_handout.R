
setwd("your wd")

source("aux_functions.R")

library(rstan)
library(HDInterval)
library(bayesplot)
library(ggplot2)

### Load data
#############

load("data_logistic_regression.RData")

### Prior Predictive checks
###########################

### Intercept only
# Start with:
# alpha ~ N(0,1)
# alpha ~ N(0,1.5)
# alpha ~ N(0,2.5)
# alpha ~ N(0,10)

par(mfrow=c(2,2), pty="s")
for (j in c(1, 1.5, 2.5, 10)) {
  
  hist(boot::inv.logit(rnorm(1000, 0, j)),
       main = paste0("a ~ N(0, ", j, ")"),
       xlab = "probability")
  abline(v = 0.5, col = "red", lty = 2)
  
}
par(mfrow=c(1,1), pty="m")

### Adding covariates

# Load Stan files and prepare data

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
  sigma_alpha = 1.5
)

rstan_options(auto_write = TRUE)

logi_reg <- stan_model("logistic_regression.stan")
logi_reg_prior <- stan_model("logistic_regression_prior.stan")

prior_sample <- sampling(logi_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)

plot_prior_point(prior_sample_fit$prob, xlab = "i", ylab = "Probability")

# Start with beta1=beta2 ~ N(0,5)

dat_list$sigma_b1 <- 5
dat_list$sigma_b2 <- 5

prior_sample <- sampling(logi_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)

plot_prior_point(prior_sample_fit$prob, xlab = "i", ylab = "Probability")

# what prior for 'slopes'? can we use intuition to define what weakly informative is?

par(mfrow=c(1,3), pty="s")
hist(boot::inv.logit(x1*rnorm(1000, 0,  5)), xlab = "probability", main = "beta1 ~ N(0,5)")
hist(boot::inv.logit(x1*rnorm(1000, 0,  1)), xlab = "probability", main = "beta1 ~ N(0,1)")
hist(boot::inv.logit(x1*rnorm(1000, 0, .5)), xlab = "probability", main = "beta1 ~ N(0,0.5)")
par(mfrow=c(1,1), pty="m")

# what if we use the same priors with larger covariate values?

par(mfrow=c(1,3), pty="s")
hist(boot::inv.logit(x1 * rnorm(1000, 0, 1)), xlab = "probability", main = "x1; beta1 ~ N(0,1)")
hist(boot::inv.logit(x1*2*rnorm(1000, 0, 1)), xlab = "probability", main = "2*x1; beta1 ~ N(0,1)")
hist(boot::inv.logit(x1*4*rnorm(1000, 0, 1)), xlab = "probability", main = "4*x1; beta1 ~ N(0,1)")
par(mfrow=c(1,1), pty="m")

# for unambiguous prior setting, muse scale covariates!

x1sc <- as.vector(scale(x1)*0.5)

par(mfrow=c(1,3), pty="s")
hist(boot::inv.logit(x1sc*rnorm(1000, 0,  5)), xlab = "probability", main = "scaled, beta1 ~ N(0,5)")
hist(boot::inv.logit(x1sc*rnorm(1000, 0,  1)), xlab = "probability", main = "scaled, beta1 ~ N(0,1)")
hist(boot::inv.logit(x1sc*rnorm(1000, 0, .5)), xlab = "probability", main = "scaled, beta1 ~ N(0,0.5)")
par(mfrow=c(1,1), pty="m")

x1sc_l <- as.vector(scale(x1*4)*0.5)

par(mfrow=c(1,3), pty="s")
hist(boot::inv.logit(x1sc_l*rnorm(1000, 0,  5)), xlab = "probability", main = "x1 scaled; beta1 ~ N(0,5)")
hist(boot::inv.logit(x1sc_l*rnorm(1000, 0,  1)), xlab = "probability", main = "2*x1 scaled; beta1 ~ N(0,1)")
hist(boot::inv.logit(x1sc_l*rnorm(1000, 0, .5)), xlab = "probability", main = "4*x1 scaled; beta1 ~ N(0,0.5)")
par(mfrow=c(1,1), pty="m")

# a reasonable option is 1!

par(pty="s")
hist(boot::inv.logit(x1sc*rnorm(1000, 0, 1)), xlab = "probability", main = "x1 scaled; beta1 ~ N(0,1)")
par(pty="m")

# the same holds for x2

x2sc <- as.vector(scale(x2)*0.5)

par(mfrow=c(1,2), pty="s")
hist(boot::inv.logit(x1sc*rnorm(1000, 0, 1)), xlab = "probability", main = "x1 scaled; beta1 ~ N(0,1)")
hist(boot::inv.logit(x2sc*rnorm(1000, 0, 1)), xlab = "probability", main = "x2 scaled; beta2 ~ N(0,1)")
par(mfrow=c(1,1), pty="m")

### Now redefine the data list accordingly!

dat_list <- list(
  N = length(y),
  x1 = x1sc,
  x2 = x2sc,
  y = y,
  mu_b1 = 0,
  mu_b2 = 0,
  sigma_b1 = 1,
  sigma_b2 = 1,
  mu_alpha = 0,
  sigma_alpha = 1
)

prior_sample <- sampling(logi_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_fit <- extract(prior_sample)

plot_prior_point(prior_sample_fit$prob, xlab = "i", ylab = "Probability")

# lesson: multiple variables nontrivially impact on the prior predictive distribution

### Estimation
##############

# dat_list$x1 <- x1
# dat_list$x2 <- x2

dat_list$sigma_b1 <- 1
dat_list$sigma_b2 <- 1

fit <- sampling(logi_reg,
                data = dat_list,
                chains = 4,
                cores = 4)

sample_fit <- extract(fit)

### Visualize posteriors

# https://stats.stackexchange.com/questions/103951/intercept-from-standardized-coefficients-in-logistic-regression

alpha <- sample_fit$alpha - (sample_fit$beta1 * (mean(x1)*0.5)/sd(x1)) - (sample_fit$beta2 * (mean(x2)*0.5)/sd(x2))
beta1 <- sample_fit$beta1 * (0.5/sd(x1))
beta2 <- sample_fit$beta2 * (0.5/sd(x2))

summary(glm(y ~ x1 + x2, family = binomial(link = "logit")))

mean(alpha); hdi(alpha)
mean(beta1); hdi(beta1)
mean(beta2); hdi(beta2)

par(mfrow=c(1,3), pty="s")
hist(alpha, main = "alpha", xlab = "draws")
hist(beta1, main = "beta1", xlab = "draws")
hist(beta2, main = "beta2", xlab = "draws")
par(mfrow=c(1,1), pty="m")

### Compute probabilities

thr <- -0.5
prob <- mean(beta2>thr)

h_data <- hist(beta2, plot = F)
h_area <- cut(h_data$breaks, c(-Inf, thr, Inf))

par(pty="s")
plot(h_data, col = c("white", "lightblue")[h_area], main = "beta1", 
     xlab = paste0("Pr(beta2 > ", thr, " | y, x1, x2) = ", round(prob, 2)))
par(pty="m")

### Compute credible intervals

rbind(quantile(alpha, c(.025, .975)),
      quantile(beta1, c(.025, .975)),
      quantile(beta2, c(.025, .975)))

### Posterior Predictive Distribution

plot_prior_point(sample_fit$prob, xlab = "i", ylab = "Probability")
points(colMeans(sample_fit$prob), col = "red", cex=.75)

ccr <- numeric(length = nrow(sample_fit$y_pred))
for(s in 1:nrow(sample_fit$y_pred)) {
  
  ccr[s] <- sum(diag(table(sample_fit$y_pred[s,], y)))/sum(table(sample_fit$y_pred[s,], y))*100
  
}

par(pty="s")
hist(ccr)
par(pty="m")

### Diagnostic
##############

library(bayesplot)

params <- c("alpha", "beta1", "beta2")

mcmc_trace(fit, pars = params)
mcmc_acf(fit, pars = params)
mcmc_pairs(fit, pars = params)

rhat(fit, pars = params)
neff_ratio(fit, pars = params)
