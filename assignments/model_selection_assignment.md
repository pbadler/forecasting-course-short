---
title: "Model selection assignment"
output:
  html_document: default
layout: post
---

### Reading ###

The goal of this assignment is to repeat Tredennick et al.'s examples of 
modeling for exploration, inference, and prediction using a different (and simpler) data set.

We will use data on the Yellowstone National Park bison herd. The data come 
from [Hobbs et al. (2015)](https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/14-1413.1),
though I downloaded a more recent version of these data from Andrew Tredennick's 
[Github site](https://github.com/atredennick/bison_forecast). You can download the
data from the course website [here](https://github.com/pbadler/forecasting-course-short/blob/master/data/YNP_bison_counts.csv). Winters in Yellowstone are long, cold and snowy; when the snow is too deep, the bison have trouble feeding, so it seems reasonable to expect some influence of winter precipitation on bison population dynamics.

We will use associated climate data from PRISM, which you can download [here](https://github.com/pbadler/forecasting-course-short/blob/master/data/YNP_prism.csv).

### Formatting the data  ###

Getting the data formatted can be the most time consuming part of any analysis, 
so I am giving you a bunch of code.

Here is what I did to get the bison data ready:
```R
library(forecast)
library(ggplot2)
library(dplyr)
library(tidyverse)

bison = read.csv("data/YNP_bison_counts.csv", stringsAsFactors = FALSE)
bison = select(bison,c(year,count.mean)) # drop non-essential columns
names(bison) = c("year","N") # rename columns

# the next few lines set up a lag N variable
tmp = bison    
tmp$year = tmp$year + 1
names(tmp)[2] = "lagN"
bison = full_join(bison,tmp)
bison = filter(bison,year > min(year) & year < max(year))  # drop incomplete observations
rm(tmp)

# add log transformed variables
bison = mutate(bison, logN = log(N))
bison = mutate(bison, loglagN = log(lagN))
````
For the weather data, I wanted to set things up so I could aggregate variables
by "climate year" (often refered to as "water year") rather than calendar year.
Bison counts are completed by August each year; weather in September - December
really shouldn't effect the population in the year of the count, but it might
affect the population in the following calendar year. So I start my "climate
year" in September (month 9):
```R
weather = read.csv("data/YNP_prism.csv", stringsAsFactors = FALSE)
weather = weather %>% separate(Date,c("year","month"), sep = '-')
weather$year = as.numeric(weather$year)
weather$month = as.numeric(weather$month)
weather$clim_year = ifelse(weather$month < 9, weather$year, weather$year + 1)
weather$clim_month = ifelse(weather$month < 9, weather$month + 4, weather$month - 8)
head(weather)
```
If we were just going to do time series modeling, we might be done formatting. But 
for some of the "understanding" and "prediction" approaches, I want to have 
a version of the data with all the weather covariates that correspond to one bison count in one row. Here is how I did that:
```R
precip_wide = weather %>% 
                select(c(clim_year,clim_month,ppt_in)) %>%  # drop all the other climate variables
                spread(clim_month,ppt_in) # 'spread' is where the magic happens

# rename months (optional, but I think it makes the data frame easier to understand)
names(precip_wide)[2:13] = paste0("ppt_",c("Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug"))
head(precip_wide)

# aggregate by season
precip_wide = mutate(precip_wide, ppt_Fall = rowSums(precip_wide[,c("ppt_Sep","ppt_Oct","ppt_Nov")]))
precip_wide = mutate(precip_wide, ppt_Win = rowSums(precip_wide[,c("ppt_Dec","ppt_Jan","ppt_Feb")]))
precip_wide = mutate(precip_wide, ppt_Spr = rowSums(precip_wide[,c("ppt_Mar","ppt_Apr","ppt_May")]))
precip_wide = mutate(precip_wide, ppt_Sum = rowSums(precip_wide[,c("ppt_Jun","ppt_Jul","ppt_Aug")]))
head(precip_wide)

# merge with bison
bison_wide = left_join(bison,precip_wide,by=c("year" = "clim_year"))
```
My example does this just for the precipitation data. 
You could repeat the same steps if you also want to play with 
the temperature or vapor pressure deficit variables.

### Questions  ###

Here are the questions you need to answer. You should email to Peter an 
R script (your code) and a pdf of your final document. 

1. Exploratory modeling:  
    a. Produce scatter plots showing the correlation between the residuals of
    an AR1 population model (see More Code below) and each of your climate covariates.
    b. Do any of the correlations look strong? Optional: Are any statistically significant?
<br><br>

2. Model for inference: Test the hypothesis that deep winter snowpack 
has a negative effect on bison population growth. You can assume that 
high precipitation in winter is a reasonable proxy for snowpack. Do your
results support the hypothesis?

3. Model for prediction: We will adapt Tredennick et al.'s analysis
of butterfly population dynamics for the Yellowstone bison case
to create a predictive model of weather effects on bison populations. 
I provide code below to help you do this.
Please compare in-sample predictions from your best model with 
predictions from a no-climate null model (we will work with out-of-sample
predictions for the next assignment). How much do the climate covariates 
improve prediction? Does it matter whether you use 
ridge regression or LASSO?

### More Code  ###

For **question 1**, I extract the residuals from a Gompertz population model
like this:
```R
mbase <- lm(logN ~ loglagN, data=bison_wide )
resids <- residuals(mbase)
```

For **question 3**, we need to do a few more things to set up
the data for the analysis.

```R
# Prepare data for glmnet ------------------------------------------------------

# response variable
y <- bison_wide$logN # population growth rate

# covariates
covar_names <- c("loglagN", grep("ppt",names(bison_wide),value=T) )
X <- as.matrix(select(bison_wide, covar_names)) # covariate matrix

# Make sure there are no NAs in the covariate matrix
test_vec <- as.numeric(apply(X, MARGIN = 2, FUN = "mean"))
nacols <- which(is.na(test_vec))
if(length(nacols) > 0) X <- X[,-nacols]

# Standaradize the covariate values
X_scaled <- scale(X, center = TRUE, scale = TRUE)
```

The next block of code runs a ridge regression, and compares the errors from
that model to the errors from a no-climate null model. Notice that the `alpha`
parameter in function 'cv.glmnet()` determines whether it is ridge regression
or LASSO.

```R
# Run ridge regression ---------------------------------------------------------
pen_facts <- c(0,rep(1, ncol(X_scaled)-1)) # penalize all covariates EXCEPT the first, loglagN

lambdas <- 10^seq(2, -2, by = -.005) # sequence of penalties to test

ridge_out <- cv.glmnet(x = X_scaled, 
                       y = y, 
                       lambda = lambdas,
                       penalty.factor = pen_facts,
                       family = "gaussian", 
                       alpha = 0, # 0 for ridge, 1 for lasso, 0.5 for elastic net 
                       standardize = FALSE, 
                       type.measure = "mse", # mean square error
                       nfolds = 6) # for cross-validation

# extract and look at the best coefficients
best_coefs = ridge_out$glmnet.fit$beta[,which(ridge_out$lambda==ridge_out$lambda.min)]
print(best_coefs)

# Predictions for training data from a null model (no climate variables)
# The null model does include density-dependence
null_preds = predict(lm(y ~ X_scaled[,1]))

# predictions for training data WITH climate variables
clim_preds = predict(ridge_out, newx=X_scaled,s="lambda.min")

# compare MAE
print(paste("MAE climate =",mean(abs(clim_preds - y))))
print(paste("MAE null =",mean(abs(null_preds - y))))
```

