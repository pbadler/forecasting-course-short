---
title: "Introduction to time-series modeling"
output: html_document
layout: page
---

Now that you know how to decompose a time-series, and 
how to evaluate autorcorrelation, we can put these concepts 
together and start modeling time-series. Let's start 
by loading the Arizona NDVI and precipitation time-series again:

```
rm(list=ls())
setwd("C:/Repos/forecasting-course-short/lectures")
library(forecast)

data = read.csv("./../data/portal_timeseries.csv", stringsAsFactors = FALSE)
head(data)
NDVI_ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
rain_ts = ts(data$rain, start = c(1992, 3), end = c(2014, 11), frequency = 12)

# Plot the data
plot(NDVI_ts)
plot(rain_ts)

```

The simplest, most basic time-series model is a random walk:

$$y = c + \epsilon_t$$

where $$\epsilon$$ is normally distributed with a constant mean
and variance. Here is how we could fit that model to the
NDVI time-series:

```
avg_model = meanf(NDVI_ts)
plot(NDVI_ts)
lines(fitted(avg_model), col = 'red')
```

But we know this is not right, because there is autocorrelation
in the time series.

```
acf(NDVI_ts)
pacf(NDVI_ts)
```

Let's build a model that takes this autocorrelation into account.
In an autoregressive model, the current state of the response variable
is related to its previous states:

$$y_t = c + b_1 y_{t-1} + b_2 y_{t-2} ... + \epsilon_t$$

We can do this with an "ARIMA" model. The "AR" stands for "AutoRegressive" 
(we will get to the rest of the acronym later).

```
arima_model = Arima(NDVI_ts, order= c(2, 0, 0))
arima_model
plot(NDVI_ts,type="l")
lines(fitted(arima_model), col = 'red')
```

How is it doing? Is there still autocorrelation in the residuals?

```
plot(resid(arima_model))
acf(resid(arima_model))
```

Yes, we see autocorrelation at 1 and 2 year lags. Perhaps this 
reflects a seasonal signal? ARIMA models can handle that.

```
seasonal_arima_model = Arima(NDVI_ts, c(2, 0, 0), seasonal = c(2, 0, 0))
seasonal_arima_model
plot(NDVI_ts)
lines(fitted(seasonal_arima_model), col = 'red')
acf(resid(seasonal_arima_model))

```
Better. What are the three numbers in the second argument of the Arima
function? 1. AR order, 2. Degree of differencing, 3. Moving average. Our
input, c(2,0,0), means autocorrelation to second order, zero differencing,
and no moving average.

You can automate fitting, and ask the computer to fit many possible 
values of season lags, AR, differencing, and MA, and pick the one that
fits best (as determined by your choice of AICc, AIC, or BIC).

```
# first try with season=FALSE
arima_model = auto.arima(NDVI_ts, seasonal = FALSE)
plot(NDVI_ts)
lines(fitted(arima_model), col='red')
acf(resid(arima_model))
Box.test(resid(arima_model))

# now try with season=TRUE
arima_model = auto.arima(NDVI_ts, seasonal = TRUE)
plot(NDVI_ts)
lines(fitted(arima_model), col='red')
acf(resid(arima_model))
Box.test(resid(arima_model))

```
We can also incorporate co-variates, the $$X$$'s:

$$y_t = c + b_1 y_{t-1} + b_2 y_{t-2} + B_1 X_1 + B_2 X_2... + \epsilon_t$$

```
rain_arima_model = auto.arima(NDVI_ts, xreg = rain_ts)
plot(NDVI_ts)
lines(fitted(rain_arima_model), col = 'blue')
lines(fitted(arima_model), col = 'red')

```

