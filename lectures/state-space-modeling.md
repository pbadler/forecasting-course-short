---
layout: nil
---

*Adapted from
the
[state space modeling activity](https://github.com/EcoForecast/EF_Activities/blob/master/Exercise_06_StateSpace.Rmd) from
Michael Dietz's
excellent
[Ecological Forecasting book](https://www.amazon.com/Ecological-Forecasting-Michael-C-Dietze/dp/0691160570)*

> JAGS needs to be installed: https://sourceforge.net/projects/mcmc-jags/files/
> rjags R package needs to be installed

# State-space model forecasting

## State space models

These models assume that observations do not prefectly represent the
true value of the underlying **latent** state variables. We model
the process with those latent, true values, and link the process
to the observations an "observation model."

```
y_t-1    y_t    y_t+1   Observation model
  |       |       |
x_t-1 -> x_t -> x_t+1   Process model
```
* In other words, we separately model
    * the process - how the system evolves in time or space
	  * the observations - observation error or indirect observations

* Our model will only include a first order autoregressive component.

## Data

* Google Flu Trends data for Florida
* [https://www.google.org/flutrends/about/data/flu/us/data.txt](https://www.google.org/flutrends/about/data/flu/us/data.txt)

```{r}
gflu = read.csv("http://www.google.org/flutrends/about/data/flu/us/data.txt",skip=11)
time = as.Date(gflu$Date)
y = gflu$Massachusetts
plot(time,y,type='l',ylab="Flu Index",lwd=2,log='y')
```

### Process model

* What is actually happening in the system
* First order autoregressive component

x_t+1 = f(x_t) + e_t

* Simple linear model is AR1:

x_t+1 = b0 + b1 * x_t + e_t


### Observation model

* Google searches aren't perfect measures of the number of flu cases 
(which are what should be changing in the process model and what we care about)
* So we model this imperfect observation

y_t = x_t + e_t

* Can be much more complicated


## Model Framework

* Models like this are not trivial to fit
* Use [JAGS (Just Another Gibbs Sampler)](http://mcmc-jags.sourceforge.net) to
  fit the model using Bayesian computational methods. The rjags library uses R to call JAGS.

```{r}
library(rjags)
```


## Model

JAGS code to describe the model is stored as a string in R. It has three sections:

1. data model
    * relates observed data (y) to latent variable (x)
    * Gaussian obs error
		  
2. process model
    * relates state of the system at $t$ to the state at $t-1$
    * random walk (x_t = x_t-1 + e_t)
		  
3. priors
    * Bayesian methods need priors (even if uninformative) as well as starting points for model fitting

```{r}
RandomWalk = "
model{
  
  #### Data Model
  for(i in 1:n){
    y[i] ~ dnorm(x[i],tau_obs)
  }
  
  #### Process Model
  for(i in 2:n){
    x[i]~dnorm(x[i-1],tau_add)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_add ~ dgamma(a_add,r_add)
}
"
```

* Data and priors are stored as a list

```{r}
data <- list(y=log(y), n=length(y),
             x_ic=log(1000), tau_ic=100,
			 a_obs=1, r_obs=1, a_add=1, r_add=1)
```

* We also store a list with initial values for the parameters.

```{r}
init <- list(list(tau_add=1/var(diff(log(y))),tau_obs=5/var(log(y))))
```

* Normally would want several chains with different starting positions to avoid
  local minima.

* Send to JAGS

```{r}
j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 1)
```

* Burn in

```{r}
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)
```

* Sample from MCMC with full vector of X's

```{r}
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_obs"),
                            n.iter = 10000)
```

* Visualize: convert the output into a matrix 

```{r}
out <- as.matrix(jags.out)
xs <- out[,3:ncol(out)] # just the predicted x's
```

* Point predictions are averages across MCMC samples

```
predictions <- colMeans(xs)
plot(time, predictions, type = "l")
points(time, y)
```

* Add prediction intervals as range containing 95% of MCMC samples

```
ci <- apply(xs, 2, quantile, c(0.025, 0.975))
polygon(cbind(c(time, rev(time), time[1]), c(ci[1,], rev(ci[2,]), ci[1,][1])),
        border = NA, col="gray") 
lines(time, predictions)
points(time, y)
```

## Forecasting

* Add NAs for values to be forecast

> Make these changes at top of script and rerun

```
y = c(gflu$Massachusetts, rep(NA, 52))
time = c(as.Date(gflu$Date), seq.Date(as.Date("2015-08-16"), as.Date("2016-08-09"), "week"))
```

## Uncertainty

* The uncertainty is partitioned between process and observation models
* Look at `tau_add` and `tau_obs` (as standard deviations)

```{r}
hist(1/sqrt(out[,1]))
hist(1/sqrt(out[,2]))
plot(out[,1], out[,2])
```
