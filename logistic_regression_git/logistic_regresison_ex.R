
setwd("your wd")

source("aux_functions.R")

library(rstan)
library(truncnorm)

### Load data
#############

load("data_logistic_regression_ex.RData")

### Load the Stan files
#######################

rstan_options(auto_write = TRUE)

logit_reg <- stan_model("logistic_regression_X.stan")
logit_reg_prior <- stan_model("logistic_regression_prior_X.stan")

### Prepare the data
####################

# put dependent variable into a vector called y

# y <- 

# put matrix of independent variables into a matrix called X. This must be a matrix,
# NOT a data.frame or tibble object!
  
# X <- 
  
### Prior Predictive checks
###########################

### Unstandardized variables

dat_list <- list(
  N = length(y),
  P = ncol(X),
  X = X,
  y = y,
  mu_b = 0,
  sigma_b = 1,
  mu_alpha = 0,
  sigma_alpha = 2
)

prior_sample <- sampling(logit_reg_prior,
                         data = dat_list,
                         algorithm = "Fixed_param")

prior_sample_pi <- prior_sample %>% spread_draws(prob[condition])

prior_sample_pi %>%
  ggplot(aes(x = condition, y = prob)) +
  geom_hex(bins = 50) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() + 
  labs(x = "individual",
       y = "Probability") +
  theme(aspect.ratio = 1,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

i <- sample(1:200)[1]

prior_sample_pi %>%
  filter(condition==i) %>%
  ggplot(aes(x = prob)) +
  geom_histogram(bins = 50) +
  theme_bw() +
  labs(x = "Probability",
       title = paste0("Simulated pi for individual i = ", i)) +
  theme(aspect.ratio = 1,
        panel.grid = element_blank())

### Estimation
##############

fit <- sampling(logit_reg,
                data = dat_list,
                chains = 4,
                cores = 4)

sample_fit <- extract(fit)

ccr <- numeric(length = nrow(sample_fit$y_pred))
for(s in 1:nrow(sample_fit$y_pred)) {
  
  ccr[s] <- sum(diag(table(sample_fit$y_pred[s,], y)))/sum(table(sample_fit$y_pred[s,], y))*100
  
}

par(pty="s")
hist(ccr)
par(pty="m")
