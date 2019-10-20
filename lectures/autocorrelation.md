---
title: "Autocorrelation"
output:
  html_document: default
layout: post
mathjax: true
---

My last lecture on time series decomposition focused on one type of
signal that may be embedded in a time series. This lecture focuses 
on a different kind of signal.

Let's start by thinking about an extreme case. Imagine a time
series created as follows: at each time step, randomly draw 
a value from a normal distribution. What would this time series 
look like? Do it look like real, biological times series?

```
rm(list=ls())
setwd("C:/Repos/forecasting-course-short/lectures")
library(forecast)
library(astsa)

# create a random time series 
set.seed(20)
whitenoise <- ts(rnorm(273,mean=0,sd=1))          
plot(whitenoise, main="White noise")

```

We call this "white noise." The value at each time step
independent of the value at the previous time. 
In most real time series, the value at t+1 has 
some dependence on the value at t. 

Now let's look at a real biological time series: NDVI
in Arizona:
```
data = read.csv('./../data/portal_timeseries.csv')
head(data)
NDVI.ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
plot(NDVI.ts, xlab = "Year", ylab="greenness", main="NDVI")

```

Why is this one so different?  For starters, we know that the NDVI 
data has seasonality in the signal. Related to that, the value 
at one time step is not independent of values at previous time
steps. 

We can explore that dependence by looking at lag plots. Lag 
plots show the correlation between the value at t and the values 
at t+h, where h is the lag. 

```
# plot all the lags up to 12 months.
lag.plot(NDVI.ts, lags=12, do.lines=FALSE)

```
## Autocorrelation functions

To visualize this pattern more succinctly and quantitatively,
for each one of these lags we can calculate a correlation 
coefficient. We can then plot the correlations as a function
of the lag. This is what an autocorrelation function (ACF) or
correlogram does.

```
acf(NDVI.ts)

```

In this case, the x axis is the proportion of the annual frequency
for the NDVI time series, but sometimes this is displayed in the
actual units of the timeseries (months in our case).
The blue lines plotted are the 95% confidence interval. If more 
than 5% of spikes are outside this bound, your time series is probably not white noise. 

A time series should always give you a correlation coefficient of 
1 at a zero time lag. Some software packages display this zero lag
value, others do not. Make sure you check before you get excited about having a strong signal in the first lag!

For our NDVI time series, the strongest correlation is at a lag 
of 1 month, and then the autocorrelation in the time series drops 
off substantially. We see little blips every 12 months,
which is the seasonality coming through. We see a very weak signal 
of a negative relationship at 6 month lags. What might that mean, biologically?

If there is a pattern to your spikes, that is usually a dependable 
sign that you have autocorrelation structure in your time series.

In contrast, here is an ACF function for white noise:
```
lag.plot(whitenoise, lag = 12, do.lines = FALSE)
acf(whitenoise)

```

Here is autocorrelation for precipitation in Portal, Arizona:
```
PPT.ts = ts(data$rain, start=c(1992,3), end = c(2014,11), frequency=12)
lag.plot(PPT.ts, lags=12, do.lines=FALSE)
acf(PPT.ts)

```

And for monthly abundance of kangaroo rats at the same site:
```
rats.ts = ts(data$rodents, start=c(1992,3), end = c(2014,11), frequency=12)
lag.plot(rats.ts, lags=12, do.lines=FALSE)
acf(rats.ts)
```
Why do you think the kangaroo rat time series shows so much more
autocorrelation than NDVI or precipitation?

Autocorrelation can echo through a time series. If $Y_t$ and $Y_{t-1}$ are strongly correlated, and $Y_{t-1}$ and $Y_{t-2}$ are strongly correlated, then presumably $Y_{t}$ and $Y_{t-2}$ must be correlated too. In fact, the correlation at lag 2 is the square of the correlation at lag 1. We can examine this by using a partial ACF.  

The PACF shows the correlation coefficient between $t$ and $t-2$ 
after accounting for the relationship between $t$ and $t-1$.

```
acf(NDVI.ts)
pacf(NDVI.ts) 
```
Note that the PACF starts at lag 1, not lag 0; this can
be confusing. 

The Forecast package has a nice feature that lets you look at 
the time series, the ACF, and the PACF at the same time:

```
# NDVI
tsdisplay(NDVI.ts)

# precipitation
tsdisplay(PPT.ts)

# kangaroo rats
tsdisplay(rats.ts)

```

The rodents show a classic signal of an autoregressive model. 
In an autoregressive model, the value of the variable at time $t$
depends on the values at previous time steps. The random
walk is a classic autoregressive model:

```
set.seed(1)
x = w = rnorm(1000)
for (t in 2:1000) x[t] = x[t-1]+w[t]
tsdisplay(x)
```

You can use these plots to get a better understanding 
of your time series. Understanding your autocorrelation 
structure is also important for doing statistics.
Applying standard regression approaches to autocorreated data 
will give underestimates of the variance, inflated test statistics,
and narrow CIs.

## Cross-correlation functions

You may be interested in how two time series are correlated across 
different lags. You can use the cross-correlation function 
to evaluate this:

```
ccf.plantsrain = ccf(PPT.ts, NDVI.ts)
plot(ccf.plantsrain)
lag2.plot(PPT.ts, NDVI.ts, 12)

ccf.plantrat = ccf(NDVI.ts, rats.ts)
plot(ccf.plantrat)
lag2.plot(NDVI.ts, rats.ts, 12)

```

## Stationarity

In a stationary time series, no moment of the distribution depends 
upon or changes predictably with time. Practically, this means 
constant mean, variance, and autocovariance.

What do stationary and non-stationary time series look like?

## Summary

Some take home messages for autocorrelation: 

1. Autocorrelation is useful in that information about the past and future states of the system is encoded in the time series. This is information that can be leveraged for forecasting.

2. Autocorrelation creates challenges for standard statistical 
analyses. Many tests assume iid: independent and identically distributed errors, meaning that your data is a random draw 
from an underlying distribution. But autocorrelation
means that your data is not a random draw. Each draw is influenced 
by the previous draw. There are approaches for dealing with 
autocorrelation that allow for traditional hypothesis testing (e.g., specifying correlations in the errors).




