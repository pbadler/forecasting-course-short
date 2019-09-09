---
title: "Model selection assignment"
output:
  html_document: default
layout: post
---

### Reading ###

This exercise is based on Tredennick et al. (in prep), "A practical guide to selecting models 
for exploration, understanding, and prediction in ecology." By now you should already have 
downloaded the manuscript from Canvas and read it. The goal of this assignment is to repeat
the paper's examples of modeling for exploration, understanding, and prediction using
a different (and simpler) data set.

### The data  ####

We will use data on the Yellowstone National Park bison herd. The data come 
from [Hobbs et al. (2015)](https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/14-1413.1),
though I downloaded a more recent version of these data from Andrew Tredennick's 
[Github site](https://github.com/atredennick/bison_forecast). You can download the
data from the course website [here](https://github.com/pbadler/forecasting-dynamics-course/blob/master/data/YNP_bison_counts.csv).

We will use associated climate data from PRISM, which you can download [here](https://github.com/pbadler/forecasting-dynamics-course/blob/master/data/YNP_prism.csv).

### Formatting the data  ###

Getting the data formatted can be the most time consuming part of any analysis, so I am
giving you a bunch of code.

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
for some of the "understanding" and "prediction" approaches, I want to have a version
of the data with all the weather covariates that correspond to one bison count in one row.
Here is how I did that:
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
My example does this just for the precipitation data. You could repeat the same steps
if you also want to play with the temperature or vapor pressure deficit variables.

### Questions  ###

Here are the questions your group needs to answer. You should submit an R script (your
code) and a pdf of your final document. If you want to be fancy, you can submit an
Rmarkdown document, but don't do it just to impress me. I give some hints and additional 
code below.

1. Exploratory modeling  
    a. Produce scatter plots showing the correlation between the residuals of
    an AR1 population model (see Hints below) and each of your climate covariates.
    Do any of the correlations look strong? Optional: Are any statistically significant?
    b. Use the `climwin` package to find the optimal aggregation window for one or more
    of the climate covariates (e.g. precipitation, temperature). Are the results consistent
    with the scatter plots?
<br><br>

2. Model for understanding: Test the hypothesis that deep winter snowpack has a negative
effect on bison population growth. You can assume that high precipitation in winter is a
reasonable proxy for snowpack.

3. Model for prediction. Adapt Tredennick's [butterfly vignette](https://github.com/atredennick/modselr/blob/master/vignettes/regularization.Rmd) 
to create a predictive model of climate effects on bison populations. Compare in-sample
"predictions" from your best model with predictions from an AR1 null model (see Hints).
How much do the climate covariates improve prediction? Does it matter whether you use 
ridge regression or LASSO (see Hints)?

### Hints  ###

For **question 1**, I extract the residuals from a Gompertz population model
like this:
```R
mbase <- lm(logN ~ loglagN, data=bison_wide )
resids <- residuals(mbase)
```
To use the climwin package, we actually want to use time-series-like versions
of the bison and weather data (not the "climate year" variables or the version
we manipulated with `spread()`). Here is how I set it up:
```R
library(climwin)
library(lubridate)

# create Date variables 
bison$Date = as_date(paste(bison$year, "08", "01"))
weather$Date = as_date(paste(weather$year,weather$month,"15"))
```
The key function in climwin is `slidingwin()`. You should check out the documentation
to learn about each argument. Here is how I used it:
```R
bisonWin <- slidingwin(xvar = list(ppt = weather$ppt_in),
              cdate = weather$Date,
              bdate = bison$Date,
              baseline = lm(logN ~ loglagN, data = bison),
              range = c(12, 0),
              type = "relative", stat = "sum",
              func = c("lin"), cmissing = FALSE, cinterval = "month")
```
And here is how I examined the results:
```R
bisonWin$combos
summary(bisonWin[[1]]$BestModel)
```
You are on your own for **question 2**!

For **question 3**, start with Tredennick's vignette. You should be able to
ignore (cut or comment out) his section "Subset out fitting data" since
our bison data are simpler--just one population. On the other hand,
you will need every line of the "Prepare data for glmnet" section. When
you are ready to fit, you should set up the penalty factors like this:
```R
pen_facts <- c(0,rep(1, ncol(X)-1)) # penalize all covariates EXCEPT the first, loglagN
```
After you run the `glmnet.cv()` function, ignore the rest of Tredennick's code
and just look at the output and generate predictions like this:
```R
# extract and look at the best coefficients
best_coefs = ridge_out$glmnet.fit$beta[,which(ridge_out$lambda==ridge_out$lambda.min)]
print(best_coefs)

# predictions for training data WITH climate variables
clim_preds = predict(ridge_out, newx=X,s="lambda.min")

# predictions from a null model (no climate variables)
null_preds = predict(lm(y ~ X[,1]))

# compare MAE
print(paste("MAE climate =",mean(abs(clim_preds - y))))
print(paste("MAE climate =",mean(abs(null_preds - y))))
```
For ridge regression, set `alpha=0` in `cv.glmnet()`; for LASSO, set
`alpha=1`.


