---
title: "Introduction to time-series forecasting"
output: html_document
layout: post
mathjax: true
---

In [Forecasting Principles and Practices](https://otexts.com/fpp2/), 
Hyndman breaks forecasting down into six steps:

1. Problem definition
2. Gather information
3. Exploratory analysis
4. Choosing and fitting models
5. Make forecasts
6. Evaluate forecasts

We jumped in at step 4; now let's move on to step 5.
The last time we fit models, the simplest was just the average:

$y_t = c + \epsilon_t$

To make a forecast from any model, we ask what the model would 
predict at the future time-step of interest. For the average model,
we just need to know what $c$ is, which is just the mean(NDVI_ts):

```
rm(list=ls())
setwd("C:/Repos/forecasting-dynamics-course/lectures")

library(forecast)
library(ggplot2)

# import data
data = read.csv("./../data/portal_timeseries.csv", stringsAsFactors = FALSE)
head(data)
NDVI_ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
rain_ts = ts(data$rain, start = c(1992, 3), end = c(2014, 11), frequency = 12)

mean(NDVI_ts)

```

We can use the function `meanf()` to get a forecast for the average model. 
The mean value is the forecast value:

```
avg_forecast = meanf(NDVI_ts)
avg_forecast
```

Let's look at the forecast object:
```
str(avg_forecast)
```

It shows:

* The method used for forecasting
* Values for fitting the model
* Information about the model
* Mean values for the forecast

The expected value, or point forecast, is in `$mean`:

```
avg_forecast$mean
```

To plot the forecast:

```
plot(NDVI_ts)
lines(avg_forecast$mean, col = 'red')

# but better to use built-in plotting functions
plot(avg_forecast)

# or using ggplot:
autoplot(avg_forecast)
```

We can change the number of time-steps in the forecast using the 
argument h:
```
avg_forecast = meanf(NDVI_ts, h = 50)
plot(avg_forecast)
```

We can also show the *uncertainty* in the forecast. This is important!
It shows how confident our forecast is.

```
avg_forecast
avg_forecast <- meanf(NDVI_ts, level = c(50, 95))
avg_forecast
plot(avg_forecast)
```

The shaded areas show the 80% and 95% confidence intervals (those 
are the defaults). Only variation in $\epsilon_t$ is included, not errors in parameters!

Does it look like 95% of the empirical points fall within the gray band?
If not, we should be skeptical of the forecast.
How do we tell? We'll come back to this in the lecture on forecast evaluation.

### Forecasting with more complex models

Obviously, we aren't limited to forecasting with the mean model. The
`forecast()` function in package forecast can generate a forecast
from any time-series model. Here is an example from a non-seasonal ARIMA:

```
arima_model = auto.arima(NDVI_ts, seasonal = FALSE)
arima_model
arima_forecast = forecast(arima_model)
plot(arima_forecast)
```

You can see that the first step is influenced strongly by the previous 
time-step, which is why it is so high above the mean. The second step 
is pulled below by the negative AR2 parameter. Over time, the forecast
gradually reverts to the mean.

Here is a forecast using a seasonal ARIMA model (the best model we found last time).
We specify a forecast 36 months into the future, and show the 80 and 99% 
prediction intervals:

```
seasonal_arima_model = auto.arima(NDVI_ts)
seasonal_arima_forecast = forecast(seasonal_arima_model, h = 36, level = c(80, 99))
plot(seasonal_arima_forecast)
```

### Forecasts from cross-sectional approach 

If we ignore temporal dependence, and treat each year as an independent observation,
we can generate forecasts from a simple linear model. This is called a 
"cross-sectional" approach (as opposed to dynamic).

```
library(dplyr)
library(tidyr)

# calculate summer rain each year
monsoon_data <- data %>%
  separate(date, c("month", "day", "year"), sep = '/') %>%
  filter(month %in% c(7, 8, 9)) %>%
  group_by(year) %>%
  summarize(monsoon_rain = sum(rain), monsoon_ndvi = mean(NDVI), monsoon_rodents = sum(rodents))

# plot summer rain
ggplot(monsoon_data, aes(x = monsoon_rain, y = monsoon_ndvi)) +
  geom_point() +
  geom_smooth(method = "lm")

# fit a linear model
rain_model = lm('monsoon_ndvi ~ monsoon_rain', data = monsoon_data)

# make and plot the forecast
rain_forecast = forecast(rain_model, newdata = data.frame(monsoon_rain = c(120, 226, 176, 244)))
plot(rain_forecast)
```
