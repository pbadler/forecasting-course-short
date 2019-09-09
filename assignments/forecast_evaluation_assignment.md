---
title: "Forecast evaluation assignment"
output:
  html_document: default
layout: post
---

### Overview  ###

The purpose of this assignment is to practice visualizing and quantifying forecast accuracy. 
You will adapt code from the [forecast evaluation lecture](../lectures/forecasting-evaluation) 
and from the lecture on Monte Carlo simulation of [prediction intervals](../lectures/prediction_intervals_via_MC). We will
work again with the Yellowstone bison data; you will compare 
the forecast accuracy of a "null" model with no climate data to a
model that incorporates some climate data.

### Preliminary steps

1. Download the same data you used for the [model selection assignment](./model_selection_assignment), and then prepare the data the same way
(creating log(N) and lag log(N) variables, and aggregating and merging the 
precipitation data to annual time scale). You should be able to copy and paste
the first chunk of the script you wrote for that assignment.

2. Now, split the full data set into a training set, including all years
up through 2011, and a test set, for years 2012-2017.

3. Fit a no-climate model to the training data set. It should look something 
like: `null_model = lm(logN ~ logN_lag, data = train_data)`.

4. Fit an alternative model that also includes at least one weather covariate
(feel free to fancy it up if you like). 
Based on the last assignment, January precipitation would be a good bet. So
this model might look something like: 
`clim_model = lm(logN ~ logN_lag + ppt_Jan, data = train_data)` .

5. Set up a Monte Carlo simulation to a) generate predictions for the 
test data period and b) simulate uncertainty due to parameter error.
Why not just use the `forecast()` function? Because we fit the models using
`lm()` instead of `Arima()` and `forecast()` won't realize that 
the lagged density term is autoregressive. (Why didn't I ask you
to do this using `Arima()`? It's a long story that I will tell you
in class. The short version: Doing it that way would make some
things easy but would make the Monte Carlo simulations much
harder.) *Assume that future (test set) climate covariates are 
known, but that "true" lag population size is known only in
year 2012. After that, use your predicted population sizes
as the lag values in subsequent years.*

Now you should be ready to evaluate your forecasts.

### Lab report

Your assignment is to answer the question: How much (if at all) do
climate covariates improve the forecasts? Your answer should be based
on three complementary ways of comparing the two models:

1. The accuracy of the point forecasts for the test data. By "point forecast,"
I mean the mean or median of the Monte Carolo simulations of bison population size 
for the test data set years.

2. Uncertainty due to parameter error for each model. (*Bonus:* 
Run another simulation to estimate uncertainty due to
process error *and* parameter error.)

3. Visualize the forecasts. Plot the observed population counts for the 
training and test data, the predictions for the test data, and the uncertainty
around the predictions due to *parameter error*. 

As usual, you should turn in both the report (text and figures) and your
R script(s).  

### Hints

* The crux of this assignment will probably be the Monte Carlo simulations.
The [prediction intervals](../lectures/prediction_intervals_via_MC)
demo gives two examples of Monte Carlo simulations, a logistic growth example, 
and a correlated errors example for a simple regression. You should start with the 
logistic growth example as a template. You will need to modify it in 
three ways:
    * First, replace $r$ and $K$ with the coefficients from your null model (or your 
  climate model), and change the logistic growth equation to the equation that 
  corresponds to the model you fit. Extract the mean values of the coefficients
  using `coef(my_model)`. 
    * Second, when you sample random values of the coefficients, you will draw
  from a multivariate normal distribution to account for correlated errors.
  To do this you will need to extract the variance-covariance
  matrix using `vcov(my_model)`, and you will replace the two `rnorm()` calls
  with one `rmvnorm()` call. You should be able to copy code from the
  correlated errors example.
    * The third change will be the hardest. In the logistic growth example, I 
  just initialized the model at some arbitrarily low value and ran forward from there.
  In this case, we want to make predictions for six years (2012 to 2017), but we want
  to start from the population size observed in 2011. So we are really starting 
  our "test set" simulation from 2011. (Later on, you will have to decide whether or 
  not to plot 2011 as a test set prediction.) Here is how I set up the `n` 
  matrix to hold my simulation results:
```
tot_time = NROW(test) + 1   # number of time steps: add one for the initial condition
n = matrix(NA,NE,tot_time)   # storage for all simulations
n[,1] = test$logN_lag[test$year==2012]     # set initial conditions to LAG popn. size in year 2012
```

  
* Once you have predictions, you can calculate their accuracy.
For `lm()` objects, the `accuracy()` in package forecast
function will only return results for the training set. To compute 
the same metrics for the test set, you will pass the function 
the point predictions from your simulation along with the observations. 
It should look something like this:
```
test_results = accuracy(my_predictions, test_data$counts)
```

* Since we are not using the `forecast()` function, you won't
be able to use the handy `plot.forecast()` function to visualize
the forecasts. You will have to build your own figure. It should 
show the observed data for the training and test period, the
point predictions for the test period, and the 95% confidence
intervals around those predictions. Extra gratitude to
whoever shows me how to do this using ggplot. 

* For the bonus problem, remember that the variance of the residuals 
is equivalent to the process error in a linear regression.
So a 95% confidence interval around a prediction would be the 
point prediction +/- two times the standard deviation of the 
residuals.





