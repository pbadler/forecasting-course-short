---
title: "Simulating uncertainty assignment"
output:
  html_document: default
layout: post
mathjax: true
---

### Overview  ###

The purpose of this assignment is to practice quantifying uncertainty using a Monte Carlo simulation approach. 

We will work once again with the Yellowstone bison data. You will fit a fairly simple model, then use the code for the Monte Carlo simulation approaches I presented in [lecture](./../lectures/prediction_intervals_via_MC) to generate forecasts and quantify various sources of uncertainty.

### Preliminary steps

1. Prepare the data the same way you did for the [model selection assignment](model_selection_assignment). This should include all the code 
up to and including the line:
```R
# merge with bison
bison_wide = left_join(bison,precip_wide,by=c("year" = "clim_year"))
```

2. Now, split the full data set into a training set, including all years
up through 2010, and a test set, for years 2011-2017.
```R
train = subset(bison_wide, year < 2011)
test = subset(bison_wide, year >= 2011)
```

3. Fit a linear model (on log scale) with density-dependence and one climate covariate:
```R
clim_model = lm(logN ~ logN_lag + ppt_Jan, data = train)
```
Now you should be ready to generate forecasts for the test set and quantify uncertainty from parameter error and process error. For these simulations, you will assume that future (test set) climate covariates are known, but that "true" lag population size is known only in year 2011. After that, use your predicted population sizes as the lag values in subsequent years.

### Questions

Use your code to produce two figures and answer one question:

1. A figure showing a) the bison population counts for the full time series (training and test years), b) the forecast means for the test years, and c) the 95% uncertainty from parameter error only.

2. A similar figure as in #1, but showing 95% uncertainty from parameter error *plus* process error.

3. Based on these two figures, what can you include about the relative contribution of parameter error and process error to overall uncertainty?

Email Peter your R script(s) and a pdf containing the figures and your answer to question 3.

### Hints

* The crux of this assignment will probably be the Monte Carlo simulations. Remember to draw you parameters from a multivariate normal distribution to account for correlated errors. You should be able to copy code from the correlated errors [example](./../lectures/prediction_intervals_via_MC).

* In the logistic growth example, we initialized the model at some arbitrarily low value and ran forward from there. In this case, we want to make predictions starting in year 2011, which means we should initialize with the population size observed in 2010. 

* Since we are not using the `forecast()` function, you won't
be able to use the handy `plot.forecast()` function to visualize
the forecasts. You will have to build your own figure. 





