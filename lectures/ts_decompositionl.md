---
title: "Time series decomposition"
output:
  html_document: default
layout: post
---

A time series can reflect different processes, each causing
fluctuations on a different scale. Consider the long-term record of 
atmospheric [CO2]:
* Human emissions cause a long-term increase 
* Seasonality causes periodic fluctuations 
* There are also stochastic (random) fluctuations at shorter scales
We may be interested in studying one of these processes, or they may be a "nuisance" we need to account for in order to study something else. Time series decomposition allows us to accomplish both goals. 

Time series decomposition breaks down a time series into trend, seasonal, and irregular fluctuations. To extract these components, we follow three steps:
1) we fit a model to all observed data to extract the long-term trend
2) we fit a second model to the remaining data to pull out the seasonal signal
3) whatever is left over are the irregular fluctuations (residuals)

## Time Series Objects in R

To do a time series decomposition in R, we need our data to be a time series object. This is a data format, like a dataframe, that has a special structure and R knows to work with it in a special way.

Some packages will require you to put your data into a time series object specific to that package. For today, we will use the standard ts object in the base package. It's limitation is that it can only take regularly spaced data (i.e. monthly, daily, quarterly, annual). There are other methods that can take irregular data and import that into a time series object. Packages that can handle irregular data include zoo and xts.

Here is an example using data on NDVI (a remotely-sensed index of 
vegetation activity) from the Sonoran Desert in Arizona, USA.

```
library(forecast)
setwd("C:/Repos/forecasting-course-short/lectures")

# import data
NDVI = read.csv('./../data/portal_timeseries.csv', stringsAsFactors = FALSE)
head(NDVI)

# convert it to time series data object
NDVI.ts = ts(NDVI$NDVI, start = c(1992, 3), end = c(2014, 11), frequency = 12)

# check it out
plot(NDVI.ts, xlab = "Year", ylab="greenness")
class(NDVI.ts)
NDVI.ts
start(NDVI.ts)
end(NDVI.ts)

# You cannot subset a ts object without losing the date info
# So we use use the window() function
str(NDVI.ts)
data.2000 = window(NDVI.ts, start=c(1999,1),end=c(2000,12))
data.2000
```
## Trend

A moving average is a common way of extracting the trend across years.  The "order" is the size of the moving window. At the start
and end of the time series, the window is truncated (small sample
size).

```
# Get moving average trend
MA_m13 = ma(NDVI.ts, order=13, centre = TRUE)
plot(NDVI.ts)
lines(MA_m13, col="blue", lwd = 3)

# try a longer window
MA_m49 = ma(NDVI.ts, order=49, centre = TRUE)
plot(NDVI.ts)
lines(MA_m49, col="blue", lwd = 3)
```
## Seasonal signal

Classic Decomposition uses a Moving Average to fit the trend. After "detrending" (removing the trend from the observed data), the next
step is to fit the seasonal signal. There are two basic ways to do this, additive or multiplicative. 
* Additive: Observed = Trend + Seasonal + Irregular (fluctuations in the time series are independent of the trend)
* Multiplicative: Observed = Trend*Seasonal*Irregular (fluctuations in the time series increase with the trend)

Let's look for a relationship between trend and seasonality in our
NDVI data:
```
Seasonal_residual_add = NDVI.ts - MA_m49
plot(Seasonal_residual_add)

Seasonal_residual_multi = NDVI.ts/MA_m49
plot(Seasonal_residual_multi)
```
There is not much difference between these two plots, probably 
because the trend is weak, so in this case the choice does not
matter.  These plots show the seasonal signal AND the "random" 
signal. The next step is to disentangle those two signals.

We pulled out the trend signal ourselves, because I wanted you to
understand what is happening. But there is software to automate
every step of the process. We will use a standard decomposition
function to pull out the seasonal signal. This approach assumes
that the seasonal signal stays constant over time, except 
for an effect of trend in the multiplicative model. Other software
packages allow changes over time in the seasonal signal 
(e.g., STL decomposition).

```
fit_add = decompose(NDVI.ts, type = 'additive')
plot(fit_add)
str(fit_add)

fit_mult = decompose(NDVI.ts, type = 'multiplicative')
plot(fit_mult)
str(fit_mult)

# It's hard to see the seasonal pattern, so let's zoom in.
# by subsetting the seasonal fits:
plot(fit_add$seasonal[11:23],type="o") # started at 11 b/c it is the first January

# Would we get the same answer just by calculating monthly means?
# Use tapply() and cycle():
monthly_means <- tapply(NDVI.ts, cycle(NDVI.ts), FUN=mean)
plot(monthly_means,type="o")

```

