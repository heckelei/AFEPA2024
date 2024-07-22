###########################################################
##                Intro Bayesian Analysis with sampling
###########################################################

###########################################################
##               First part relates to class presentation
###########################################################


## Clear workspace 
rm(list = ls())

# Change working directory (This needs to be adjusted to you file system)
setwd("c:/temp/PP_summer_school")

### Estimation problem and the sampling distribution
# Suppose we want to estimate the population share in country AFEPISTAN with
# blood type "A+" by using a random sample of size N. We have yi = 1 if the 
# individual has type "A" and yi = 0 if not. Such a sampling follows a
# binomial distribution. It tells us "what is the probability of having N1
# people with yi = 1 in a sample of size N if the share in the population with
# blood type "A+" is = p.

### Generating a sample from the sampling distribution and estimate p
# generate a sample of size N = 100 for a true unknown p=0.3. How would you
# estimate the unknown p with the sample? Intuitively, you choose the share in
# the sample as your estimate of p.
N = 100
y = rbinom (N,1,0.3)
N1 = sum(y)
p_hat = N1/N
y
p_hat


### From the sampling distribution to the likelihood function
# The estimator above is the maximum of the likelihood function. But what is 
# the likelihood function? It is telling us how likely it is to observe a certain
# sample for different types of parameter values, here values of p. Let's
# illustrate what that means. Suppose we draw just a sample of N=3. What are the
# possible outcomes?
pot_outcomes = rbind(c(0,0,0),c(0,1,0),c(0,0,1),c(0,1,1),c(1,0,0),c(1,1,0),
                     c(1,0,1),c(1,1,1))
pot_outcomes

# Now suppose we observe (0,1,1). What is the probability to observe this
# for different shares of blood type A+ in the population? 
# It is p*yi^N1 * (1-p)(1-yi)^(N-N1), so we get for our observation
# p^2*(1-p) (we have to call p "x" for the following function)

curve(x^2*(1-x)^1, from=0, to=1)

# We have just drawn the likelihood function telling us how likely it is to
# observe our data for different values of p.

### Moving from the likelihood function to the posterior.
# Multiplying a prior distribution with the likelihood gives an expression
# that is proportional to the posterior distribution. We can make this to a
# proper distribution by multiplying it with a constant such that the area
# under the curve is equal to 1.
# If we have a uniform prior U(p)=1, then the posterior post(p) is a 
# beta distribution with parameters alpha=N+1 and beta=N-N1+1. 
# So we get Beta(4,2) for our case of N=3 and N1=2 when observing (0,1,1). 
# Note that the posterior would be the same if we observed (1,1,0) or (1,0,1).

pos_samp <- rbeta(100000, 3, 2)                              
hist(pos_samp)                                                        
curve(dbeta(x, 3, 2), from=0, to=1)

### Larger sample size
# Sample size N=300 and 104 individuals with blood type A+ observed:

pos_samp <- rbeta(100000, 105, 197)                              
hist(pos_samp)                                                        
curve(dbeta(x, 105, 197), from=0, to=1)

# smallest 90% credible interval. And intuitive way to calculate (not the 
# smartest) is to use the cumulative distribution function of the posterior and
# play around with the bounds of the interval. For the smallest interval of a 
# unimodal posterior, the bounds should be around the mode and the density 
# values of upper and lower bound have to be the same:
lbd = 0.302 # lower bound of interval
ubd = 0.393 # upper bound of interval
dbeta(lbd, 105, 197)
dbeta(ubd, 105, 197)
prob_ci = pbeta(ubd, 105, 197)-pbeta(lbd, 105, 197)
prob_ci

###########################################################
##               Some exercises to play around after intro
###########################################################

### Exercise 1
# a) Generate a random sample from blood type tests of 240 individuals. The share 
#    p of blood type A+ in the population is 0.27.
# b) derive and plot the likelihood function of your sample
# c) derive and plot the posterior distribution of p
# d) According to your posterior: What is the probability that p < 0.29?
# e) According to your posterior: what is the probability that p > 0.25?
# f) What is the smallest 94% credible interval?  

### Exercise 2 - More challenging coding-wise since code not set up above 
### Drawing an approximation of the likelihood function using the sampling
### distribution for different values of p 
# For this, vary p from 0 to 1 in steps of 0.1 and draw many samples of 
# size N=30 for each p. Then calculate the probability to observe an outcome
# with N1=10 (i.e. having ten yi = 1) for each p. Finally, plot the
# sampled probabilities against p.



