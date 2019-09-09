---
title: "Computing prediction intervals with Monte Carlo simulations"
output:
  html_document: default
layout: post
---

For time series models, and other simple linear models, the
`forecast()` function computes prediction intervals and add them to your
figure. Super easy! But don't be lulled into a false sense of security:
for many kinds of models, computing predictions intervals is a challenge.

In chapter 2 of his book, Mike Dietze introduces a useful tool for computing
your own predictions intervals: Monte Carlo simulation. He illustrates this with
a simple model of logistic population growth. Chapter 11 provides a full treatment
of uncertainty analysis.

## Logistic growth example

You can find Mike's full example [here](https://github.com/EcoForecast/EF_Activities/blob/master/Exercise_02_Logistic.Rmd) along with useful explanations of 
working with probability distributions in R. Let's just go to the key block of code 
where he shows how to account for *parameter error* in the logistic growth equation.

```
# Specify uncertainty in the parameters:
r = 1          ## Intrinsic growth rate
K = 10         ## carrying capacity
r.sd = 0.2     ## standard deviation on r
K.sd = 1.0     ## standard deviation on K

# Simulation parameters:
NE = 1000      ## Ensemble size
NT = 200       ## Time steps in each run
n0 = 0.1         ## Initial population size

# Now run the logistic growth model 1000 times, each time with slightly 
# different parameters. Store all 1000 trajectories in a matrix. 
n = matrix(n0,NE,NT)   # storage for all simulations
rE = rnorm(NE,r,r.sd)  # sample values of r
KE = rnorm(NE,K,K.sd)  # sample values of K
for(i in 1:NE){        # loop over r and K samples
  for(t in 2:NT){      # for each sample, simulate throught time
    n[i,t] = n[i,t-1] + rE[i]*n[i,t-1]*(1-n[i,t-1]/KE[i])
  }
}

# calculate the median and 95% CI limits for each time point
n.stats = apply(n,2,quantile,c(0.025,0.5,0.975))
```
And a couple of figures to visualize what we just did:
```
# plot 50 runs from the ensemble
matplot(1:NT,t(n[1:50,]),type="l",col="black",lty=1)

# plot the median
plot(1:NT,n.stats[2,],type="l",
  ylim=c(0,2*K))  # leave room on y axis for CIs
# add upper and lower CI's
lines(1:NT,n.stats[1,],col="blue",lty="dashed")
lines(1:NT,n.stats[3,], col="blue",lty="dashed")
```
## Correlated parameters

In the previous example, we assumed that $r$ and $K$ were both normally-distributed
but *independent* of each other. Often, parameters will be correlated. We can
do Monte Carlo simulations with correlated parameters by using a multivariate
distribution. 

A univariate normal distribution is defined by a single mean and variance. 
A multivariate normal distribution is defined by a vector of means and
a variance-covariance matrix. The diagonal of this matrix gives the 
variances, and the off-diagonals give the covariances between the random
variates. Here is some R code to show how this works:

```
library(mvtnorm)

my_mu = c(1,10)
my_sigma = cbind(c(1,0),c(0,5))  # no correlations
print(my_sigma)
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)

my_sigma = cbind(c(1,2),c(2,5))  # positive correlations
print(my_sigma)
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)

my_sigma = cbind(c(1,-2),c(-2,5))  # negative correlations
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)
```
In linear regressions, estimates of intercepts and slopes are often
correlated. Here is an example of a Monte Carlo simulation to 
take this into account:

```
# simulate an independent variable and a response 
x = seq(1,100,5)
y = 0.8*x + rnorm(length(x),0,max(x)/5)  # rnorm() adds some noise 
plot(x,y)
reg = lm(y~x)

# extract estimates of parameter means and var-cov matrix
mu = coef(reg)
sigma = vcov(reg)

# Monte Carlo simulations
NE = 100      # Ensemble size
n = matrix(NA,NE,length(x))   # storage for all simulations
coef_samples = rmvnorm(NE,mean=mu,sigma=sigma)  # sample values of coefficients
for(i in 1:NE){        # loop over coefficient samples
  n[i,] = coef_samples[i,1] + coef_samples[i,2]*x   # predict values at each x
}

# calculate the median and 95% CI limits for each time point
n.stats = apply(n,2,quantile,c(0.025,0.5,0.975))

# plot the data
plot(x,y)
# add predictions, upper and lower CI's
lines(x,predict(reg))
lines(x,n.stats[1,],col="blue",lty="dashed")
lines(x,n.stats[3,], col="blue",lty="dashed")
```
Why does this confidence interval look so narrow compared to the scatter 
in the observations? 

It's only showing the uncertainty caused by parameter error. 
We could add the process error on top. For a 
linear model like this, it is just the variance of the errors (or residuals).




