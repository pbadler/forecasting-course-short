
library(mvtnorm)

my_mu = c(1,10)
my_sigma = cbind(c(1,0),c(0,5))  # no correlations
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)

my_sigma = cbind(c(1,2),c(2,5))  # positive correlations
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)

my_sigma = cbind(c(1,-2),c(-2,5))  # negative correlations
out = rmvnorm(100,mean=my_mu,sigma=my_sigma)
plot(out)


# simulate an independent variable and a response 
x = 1:100
y = 0.8*x + rnorm(length(x),0,max(x)/5)  # rnorm() adds some noise 
plot(x,y)
reg = lm(y~x)

# extract estimates of parameter means and var-cov matrix
mu = coef(reg)
sigma = vcov(reg)
plot(rmvnorm(1000,mu,sigma ))

# Monte Carlo simulation

# Simulation parameters:
NE = 100      ## Ensemble size

# Generate predictions for each value of x using different parameter samples
n = matrix(NA,NE,length(x))   # storage for all simulations
coef_samples = rmvnorm(NE,mean=mu,sigma=sigma)  # sample values of coefficients
for(i in 1:NE){        # loop over r and K samples
  n[i,] = coef_samples[i,1] + coef_samples[i,2]*x   # predict values at each x
}

# calculate the median and 95% CI limits for each time point
n.stats = apply(n,2,quantile,c(0.025,0.5,0.975))

true_PI = predict(reg,interval="prediction", level=0.95)

# plot the median
plot(x,y)

# add predictions, upper and lower CI's
lines(predict(reg))

# truth
lines(x,true_PI[,2],col="blue")
lines(x,true_PI[,3],col="blue")

# mine 
lines(x,n.stats[1,],col="blue",lty="dashed")
lines(x,n.stats[3,], col="blue",lty="dashed")

# try package forecast functions
lm_forecast = forecast(reg,newdata=data.frame(x=c(10,20,30)))


###
### figure out Arima constants -----------------------------------------
###

library(forecast)

# function to generate an AR1 time series
ar1 <- function(time_steps,initial_n,a,b,sigma){
  N <- numeric(time_steps)
  N[1] = initial_n
  for(i in 2:time_steps){
    N[i] = a + b*N[i-1] +rnorm(1,0,sigma)
  }
  return(N)
}

N <- ar1(time_steps=200, initial_n=1, a=5, b=0.5, sigma=0.5)
plot(N,type="l",xlab="Time")
abline(h=5/0.5,col="red")

# look at Arima fits
m1 = Arima(N,order=c(1,0,0))
m1
print(coef(m1)[2]*(1-coef(m1)[1])) # calculate "true" intercept

# better when we only use stationary portion
m2 = Arima(N[50:200],order=c(1,0,0))
m2
print(coef(m2)[2]*(1-coef(m2)[1])) # calculate "true" intercept

###
### figure out ArimaX constants -----------------------------------------
###

library(forecast)

# function to generate an AR1_X time series
ar1 <- function(time_steps,initial_n,a,b,betax,x,sigma){
  N <- numeric(time_steps)
  N[1] = initial_n
  for(i in 2:time_steps){
    N[i] = a + b*N[i-1] +rnorm(1,0,sigma) + betax*x[i]
  }
  return(N)
}

time_steps=500

x = rnorm(time_steps, 0, 1)

N <- ar1(time_steps=time_steps, initial_n=1, a=5, b=0.5, betax=0.1, x=x,sigma=0.5)
plot(N,type="l",xlab="Time")
abline(h=5/0.5,col="red")

# look at Arima fits
m1 = Arima(N,order=c(1,0,0),xreg=x)
m1
print(coef(m1)[2]*(1-coef(m1)[1])) # calculate "true" intercept

# use only stationary portion
m2 = Arima(N[(time_steps/4):time_steps],order=c(1,0,0),xreg=x[(time_steps/4):time_steps])
m2
print(coef(m2)[2]*(1-coef(m2)[1])) # calculate "true" intercept

