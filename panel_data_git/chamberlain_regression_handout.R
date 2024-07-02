
library(tidyverse)
library(rstan)

setwd("~/Library/CloudStorage/OneDrive-UniversitàCattolicadelSacroCuore/1_Lavoro_personale/2024/AFEPA_Summer_School")

source("aux_functions.R")

### Load the data
#################

dat <- read.csv("panel_fadn.csv")

### Prepare the data (standardization)
######################################

dat_tr <- dat %>%
  mutate(forest_land_sh = forest_land / cult_land,
         earn_ha = earn / cult_land,
         kw_machinery_ha = kw_machinery / cult_land,
         workers_ha = workers / cult_land)

X <- dat_tr %>% 
  select(cult_land, forest_land_sh, kw_machinery_ha, workers_ha) %>% 
  mutate_all(scale) %>%
  as.matrix()

U_chmb <- dat_tr %>% 
  select(year, cult_land, forest_land_sh, kw_machinery_ha, workers_ha) %>% 
  group_by(year) %>%
  pivot_wider(names_from = year, 
              values_from = cult_land:workers_ha) %>%
  unnest(everything()) %>%
  mutate_all(scale) %>%
  as.matrix()

std_earn <- as.vector(scale(dat$earn))

par(pty="s")
hist(std_earn)
par(pty="m")

### Prior Elicitation
#####################

rstan_options(auto_write = TRUE)
chmb_prior <- stan_model("chamberlain_regression_prior_1.stan")
chmb_prior_re <- stan_model("chamberlain_regression_prior_2.stan")
chmb_prior_re_sl <- stan_model("chamberlain_regression_prior_3.stan")
chmb_prior_re_sl_b <- stan_model("chamberlain_regression_prior_4.stan")

# 1. Variance (use the same priors seen for linear regression)

dat_list <- list(
  y = std_earn,
  X = X,
  U = U_chmb,
  J = length(unique(dat$id)),
  N = nrow(dat),
  K = ncol(X),
  L = ncol(U_chmb),
  jj = dat$id,
  sd_sigma_y = 1,
  sd_inter = 0,
  mu_inter = 0
)  

fit_prior <- sampling(chmb_prior,
                      data = dat_list,
                      algorithm = "Fixed_param")
fit_prior_smpl <- extract(fit_prior)
y_sim <- fit_prior_smpl$y

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 2. Prior on global slopes (beta), beta ~ N(0, sd_beta)
# (again, use the same priors seen for linear regression)

dat_list$sd_beta <- 1

fit_prior_re_sl_b <- sampling(chmb_prior_re_sl_b,
                              data = dat_list,
                              algorithm = "Fixed_param")
fit_prior_smpl <- extract(fit_prior_re_sl_b)
y_sim <- fit_prior_smpl$y

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 3. Prior on Individual heterogeneity, a_i ~ N(0 sd_sigma_a)

dat_list$sd_sigma_a <- 1

fit_prior_re <- sampling(chmb_prior_re,
                         data = dat_list,
                         algorithm = "Fixed_param")
fit_prior_smpl <- extract(fit_prior_re)
y_sim <- fit_prior_smpl$y

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# 5. Prior on slope coefficients in individual heterogeneity covariates
# g_l ~ N(0, sigma_g) sigma_g ~ N+(0, sd_sigma_g)

dat_list$sd_sigma_g <- 1

fit_prior_re_sl <- sampling(chmb_prior_re_sl,
                              data = dat_list,
                              algorithm = "Fixed_param")
fit_prior_smpl <- extract(fit_prior_re_sl)
y_sim <- fit_prior_smpl$y

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# introducing individual heterogeneity covariates makes the distribution leptokurtic
# correct sd_sigma_g to .25 to reduce kurtosis

dat_list$sd_sigma_g <- .25

fit_prior_re_sl <- sampling(chmb_prior_re_sl,
                            data = dat_list,
                            algorithm = "Fixed_param")
fit_prior_smpl <- extract(fit_prior_re_sl)
y_sim <- fit_prior_smpl$y

par(mfrow=c(1,3), pty="s")
hist(apply(y_sim, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_sim, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_sim, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

# At the end, the normal model for y is perhaps too restrictive as it cannot
# pick up the asymmetry in the earning distribution. Will try to fit the model
# anyway and see how it model fits turn out.

### Estimation
##############

dat_list <- list(
  y = std_earn,
  X = X,
  U = U_chmb,
  J = length(unique(dat$id)),
  N = nrow(dat),
  K = ncol(X),
  L = ncol(U_chmb),
  jj = dat$id,
  sd_inter = 0,
  mu_inter = 0,
  sd_sigma_a = 1,
  sd_sigma_y = 1,
  sd_sigma_g = .25,
  sd_beta = 1
)

rstan_options(auto_write = TRUE)

chmb <- stan_model("chamberlain_regression.stan")

fit <- sampling(chmb,
                data = dat_list,
                chains = 4,
                cores = 4)

### Posterior of beta (compare to standard fe estimator)

sample_fit <- extract(fit)
beta <- sample_fit$beta

fe_reg <- plm::plm(earn ~ cult_land + forest_land_sh + kw_machinery_ha + workers_ha,
                   data = dat_tr %>% 
                     mutate_at(vars(earn, cult_land, forest_land_sh, 
                                    kw_machinery_ha, workers_ha), scale))

t(round(apply(beta, 2, quantile, c(.025, .975)), 3))
round(confint(fe_reg), 3)

### Map beta back to the original scale od the data

X_unsc <- dat_tr %>% 
  select(cult_land, forest_land_sh, kw_machinery_ha, workers_ha) %>% 
  as.matrix()

sc_factor <- sd(dat$earn)/apply(X_unsc, 2, sd)

beta_backsc <- sweep(beta, 2, sc_factor, "*")

fe_reg_unsc <- plm::plm(earn ~ cult_land + forest_land_sh + kw_machinery_ha + 
                          workers_ha, data = dat_tr)

t(round(apply(beta_backsc, 2, quantile, c(.025, .975)), 3))
round(confint(fe_reg_unsc), 3)

### Alpha

alpha <- sample_fit$alpha

data.frame(alpha) %>%
  pivot_longer(everything(), values_to = "individual") %>%
  mutate(ind = as.numeric(gsub("X", "", name))) %>%
  group_by(name) %>%
  summarise(low = quantile(individual, .025),
            upp = quantile(individual, .975),
            med = quantile(individual, .5)) %>%
  ggplot(aes(x = name)) +
  geom_point(aes(y = med)) +
  geom_errorbar(aes(ymin = low, ymax = upp)) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  ylab("Individual Heterogeneity") +
  xlab("Individual") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_line())

### Posterior Predictive Distribution

y_rep <- sample_fit$y_rep
plot_prior_dens(y_rep, std_earn, main = "PPD")

par(mfrow=c(1,3), pty="s")
hist(apply(y_rep, 1, max), main = "max", xlab = "draws")
abline(v=max(std_earn),col="red", lwd=2)
hist(apply(y_rep, 1, min), main = "min", xlab = "draws")
abline(v=min(std_earn),col="blue", lwd=2)
hist(apply(y_rep, 1, sd), main = "sd", xlab = "draws")
abline(v=sd(std_earn),col="green", lwd=2)
par(mfrow=c(1,1), pty="m")

### Diagnostic
##############

library(bayesplot)

params <- c(paste0("beta[", 1:dat_list$K, "]"),
            paste0("gamma[", 1:dat_list$L, "]"),
            "sigma_y", "sigma_g", "sigma_a")

mcmc_trace(fit, pars = params, facet_args = list(nrow = 8))
mcmc_acf(fit, pars = params, lags = 10, facet_args = list(ncol = 2))

hist(rhat(fit, pars = params))
hist(neff_ratio(fit, pars = params))
