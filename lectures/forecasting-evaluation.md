---
title: "Forecast evaluation"
output: html_document
layout: post
mathjax: true
---

## Steps in forecasting

1. Problem definition
2. Gather information
3. Exploratory analysis
4. Choosing and fitting models
5. Make forecasts
6. **Evaluate forecasts**

## Setup

Let's start by loading the NDVI data.

```R
rm(list=ls())
library(forecast)
library(ggplot2)

data = read.csv("data/portal_timeseries.csv", stringsAsFactors = FALSE)
head(data)
NDVI_ts = ts(data$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)
plot(NDVI_ts)
```

## Model fitting

Last time we fit time-series models to the NDVI data, we used all 
available observations. This time, we are going to separate the 
dataset in two. We will have a *training* set, used to fit the model,
and a *test* set which we hold out and will use to validate, or 
evaluate, the skill of the model's predictions.

The next two lines of code subset the full time-series 
into a training set and a test set.
```R
NDVI_train <- window(NDVI_ts, end = c(2011, 11))
NDVI_test <- window(NDVI_ts, start = c(2011, 12))
```

Now we fit a model using the training set and the 
`auto.arima()` function. We will use a non-seasonal
model even though we know it is not very good.
```R
arima_model = auto.arima(NDVI_train, seasonal = FALSE)
```

We can then generate a forecast from that model.
```R
arima_forecast = forecast(arima_model, h = 36)
```

And we can visualize the time-series and the forecast:
```R
plot(arima_forecast)
```

### Compare observed and predicted

Here is where the forecast evaluation starts: we compare the 
predictions to the observations in the test set. We start
by just visualizing the comparison:
```R
# time series
plot(arima_forecast)
lines(NDVI_test)

# observed vs predicted
plot(as.vector(arima_forecast$mean), as.vector(NDVI_test))
abline(0, 1)
```

Next, we can quantify the differences between the predictions
and observations using the `accuracy()` function in the forecast
package:
```R
arima_accur = accuracy(arima_forecast, NDVI_test)
arima_accur
```

Notice that the errors are higher for the test than training data. Does this make
sense? The `accuracy` function returns the following metrics:

  * ME: Mean error
  * RMSE: Root mean squared error, this is a common metric for evaluating
  forecasts. Sometimes it is called the Brier Score.
  * MAE: Mean absolute error
  * MPE: Mean percentage error
  * MAPE: Mean absolute percentage error
  * MASE: Mean absolute scaled error
  * ACF1: Autocorrelation of errors at lag 1

Sometimes people will report the correlation between observations and predictions,
but this doesn't capture bias, or the $R^2$ of observations regressed 
on predictions (informative if compared to a null model). 
    
### Visualize and quantify the accuracy of the *seasonal* ARIMA model

Same steps, but a (little bit) better model:
```R
seasonal_arima_model = auto.arima(NDVI_train)
seasonal_arima_forecast = forecast(seasonal_arima_model, h = 36)
plot(seasonal_arima_forecast)
lines(NDVI_test)
plot(as.vector(seasonal_arima_forecast$mean), as.vector(NDVI_test))
abline(0, 1)
seasonal_accur <- accuracy(seasonal_arima_forecast, NDVI_test)
seasonal_accur
```

### Coverage

We quantify "coverage" to make sure our representation of uncertainty is accurate.
We want to make sure that, for example, roughly 95% of the 
observations fall within the 95% confidence intervals.

```R
in_interval <- arima_forecast$lower[,1] < NDVI_test & arima_forecast$upper[,1] > NDVI_test
coverage <- sum(in_interval) / length(NDVI_test)
```

Let's compare coverage of the non-seasonal and seasonal forecasts:
```R
data.frame(arima = arima_accur[2,], seasonal = seasonal_accur[2,])
in_interval_season <- seasonal_arima_forecast$lower[,1] < NDVI_test & seasonal_arima_forecast$upper[,1] > NDVI_test
coverage_season <- sum(in_interval_season) / length(NDVI_test)
coverage
coverage_season
```

### Forecast horizon

The forecast horizon is the time-scale of the forecast: How far into 
the future are we trying to predict? Typically, forecast errors increase with longer forecast horizons:
```R
plot(sqrt((arima_forecast$mean - NDVI_test)^2))
lines(sqrt((seasonal_arima_forecast$mean -  NDVI_test)^2), col = 'blue')
```

