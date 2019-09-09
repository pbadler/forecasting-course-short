---
title: "Relating time series models to population models"
output:
  html_document: default
layout: post
---

We've introduced autoregressive models in a purely statistical framework,
but they can be interpreted as population models too. Take the first order
autoregressive (AR1) model:

$x_{t+1} = a + bx_t +\epsilon_t$ .

Let's replace $x$ with $N$, which we often use as the symbol for
population size. In addition, let's ignore the error term, set $b=1$, 
and re-order the terms:

$N_{t+1} = N_t + a$ .

Now it looks more like a population model, but it's an odd one, because 
it is *additive*: population size next year is population size this year
plus some constant $a$. In other words, the population will increase
linearly with time; it will not display the exponential growth we 
typically expect for a biological system.

Let's prove this to ourselves with a short R script (cut and 
paste the code block into R).

```
# An AR1 function to simulate popn growth
ar1 <- function(time_steps,initial_n,a,b,sigma){
  N <- numeric(time_steps)
  N[1] = initial_n
  for(i in 2:time_steps){
    N[i] = a + b*N[i-1] +rnorm(1,0,sigma)
  }
  return(N)
}

# Run it with no stochasticity, no density dependence
N <- ar1(time_steps=100, initial_n=1, a=2, b=1, sigma=0)
plot(N,type="l",xlab="Time")

```
Add some stochasticity:

```
N <- ar1(time_steps=100, initial_n=1, a=2, b=1, sigma=2)
plot(N,type="l",xlab="Time")

```
And now we can add some negative density-dependence by 
setting $b<1$. The population will reach equilibrium
at $a/b$. This is easiest to see if we remove stochasticity.

```
N <- ar1(time_steps=100, initial_n=1, a=5, b=0.5, sigma=0)
plot(N,type="l",xlab="Time")
abline(h=5/0.5,col="red")

```
The lag 1 autocorrelation $b$ is directly related to density-dependence!

This is all great, but what if we want to use the time series approach
to capture more typical exponential growth dynamics? The Gompertz model makes
this not just possible, but easy. Here is the Gompertz population model:

$N_{t+1} = N_t exp (a - clogN_t + \epsilon_t)$

This model has the exponential growth we expect, but it looks 
nothing like the AR1 model, at least not yet. First we log transform
population size: $x_t = log N_t$. Then we can rewrite the model as

$x_{t+1} = x_t + a - cx_t + \epsilon_t$

which simplifies to

$x_{t+1} = a + (1-c)x_t + \epsilon_t$ .

Define $b = 1 - c$ and you get something that should look familiar:

$x_{t+1} = a + bx_t + \epsilon_t$

So the Gompertz model is an AR1 model on the log scale. 
We can use the same AR1 function to simulate this model, we 
just have to backtransform the output to put it on the arithmetic
scale.


```
N <- ar1(time_steps=100, initial_n=1, a=2, b=0.5, sigma=1)
plot(exp(N),type="l",xlab="Time")

```
How do the dynamics of this Gompertz model differ from the 
those of the AR1 on arithmetic scale?

### Dealing with ARIMA constants

After I wrote this up, I realized that the way `Arima()` is implemented
in R creates a couple of complications. `Arima()` assumes that the
intercept of the AR1 model (coefficient $a$ in the notation above)
is equal to the mean of the time series. If the time-series you
are fitting is stationary, this isn't a big deal. You can calculate
$a$ as follows:
```
a = coef(ar1_model)[2]*(1 - coef(ar1_model[1])
```
where `coef(ar1_model[1]` is the autoregressive term and
`coef(ar1_model[2]` is the mean. 

However, based on my own little experiments,
if your time-series is not stationary, and you have not
included the moving average or differencing terms needed to make
it stationary, then your estimates of $a$ and $b$ can be way off.
So if you want to interpret your parameters in a population growth
context, I recommend avoiding `Arima()` and fitting the lag density 
term directly using a linear model you specifcy yourself.


