---
title: "Forecast evaluation assignment"
output:
  html_document: default
layout: post
mathjax: true
---

### Overview  ###

The purpose of this assignment is to practice visualizing and quantifying forecast accuracy. The starting point is your code from the last assignment, 
[introduction to forecasting](intro_to_forecasting). On that assignment,
you asked how information on NDVI or rainfall could improve
a forecast of rodent population size. You assessed that qualitatively.
Now we will evaluate forecast accuracy quantitatively.

### Preliminary steps

1. Split the full data set into a training set, including all years
up through 2010, and a test set, for years 2011-2014. You can split the data
before creating the ts objects, or after. At the end of this step, you should 
have six ts objects: rats_train, rats_test, NDVI_train, NDVI_test, rain_train, rain_test.

3. Fit a no-climate ARIMA model to the rats training data set. This will be your null model.

4. Fit an alternative model that includes NDVI as a covariate (use the xreg
argument).

5. Fit a second alternative model that includes rain as a covariate.

6. Evaluate the forecasts with all three models. First you will generate 
predictions for the test set, then use `accuracy()` to evaluate our 
predictions. The examples I gave you in class did not show how to 
generate a forecast for a time-series model with a covariate. To do that,
you will need to specify the covariate in the forecast function. For example:
`my_forecast <- forecast(my_model,h = 4, xreg = NDVI_test)`.

### Lab report

Your assignment is to answer the question: How much (if at all) do
the NDVI or rain covariates improve the forecasts? Your answer should be based
on two complementary ways of comparing the two models:

1. The accuracy of the point forecasts for the test data. 

2. Visualize the forecasts. Plot the observed population counts for the 
training and test data, the predictions for the test data, and the uncertainty
around the predictions. 

As usual, you should turn in both the report (text and figures) and your
R script(s).  





