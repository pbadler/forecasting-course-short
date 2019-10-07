
# Flu state-space model, based Exercise 6 from Dietze's book

rm(list=ls())

library(rjags)

gflu = read.csv("http://www.google.org/flutrends/about/data/flu/us/data.txt",skip=11)
time = as.Date(gflu$Date)
y = gflu$Massachusetts
plot(time,y,type='l',ylab="Flu Index",lwd=2,log='y')

# To generate forecasts, add NA's to time series
# y = c(gflu$Massachusetts, rep(NA, 52))
# time = c(as.Date(gflu$Date), seq.Date(as.Date("2015-08-16"), as.Date("2016-08-09"), "week"))

# JAGS model
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

data <- list(y=log(y), n=length(y),
             x_ic=log(1000), tau_ic=100,
			 a_obs=1, r_obs=1, a_add=1, r_add=1)

init <- list(list(tau_add=1/var(diff(log(y))),tau_obs=5/var(log(y))))

j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 1)

# burn in 
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs"),
                            n.iter = 1000)
plot(jags.out)

# sample from fitted model
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x","tau_add","tau_obs"),
                            n.iter = 10000)

# visualize
out <- as.matrix(jags.out)
xs <- out[,3:ncol(out)] # just the predicted x's

# point predictions
predictions <- colMeans(xs)
plot(time, predictions, type = "l",col="red")
points(time, log(y))

# 95% prediction intervals
ci <- apply(xs, 2, quantile, c(0.025, 0.975))
polygon(cbind(c(time, rev(time), time[1]), c(ci[1,], rev(ci[2,]), ci[1,][1])),
        border = NA, col="gray") 
lines(time, predictions, col="red")
points(time, log(y))

# Uncertainty
# The uncertainty is partitioned between process and observation models
# Look at `tau_add` and `tau_obs` (as standard deviations)

hist(1/sqrt(out[,1]))
hist(1/sqrt(out[,2]))
plot(out[,1], out[,2])

