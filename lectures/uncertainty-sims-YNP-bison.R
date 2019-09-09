
rm(list=ls())
setwd("~/model_selection")

library(forecast)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mvtnorm)

# Set up bison data -----------------------------------------------------
bison = read.csv("YNP_bison_counts.csv", stringsAsFactors = FALSE)
bison = select(bison,c(year,count.mean)) # drop non-essential columns
names(bison) = c("year","N") # rename columns

# # the next few lines set up a lag N variable
tmp = bison
tmp$year = tmp$year + 1
names(tmp)[2] = "lagN"
bison = full_join(bison,tmp)
bison = filter(bison,year > min(year) & year < max(year))  # drop incomplete observations
rm(tmp)

# add log transformed variables
bison = mutate(bison, logN = log(N))
bison = mutate(bison, loglagN = log(lagN))

# Set up weather data ----------------------------------------------------
weather = read.csv("YNP_prism.csv", stringsAsFactors = FALSE)
weather = weather %>% separate(Date,c("year","month"), sep = '-')
weather$year = as.numeric(weather$year)
weather$month = as.numeric(weather$month)
weather$clim_year = ifelse(weather$month < 9, weather$year, weather$year + 1)
weather$clim_month = ifelse(weather$month < 9, weather$month + 4, weather$month - 8)
head(weather)

# To prepare for a merge with the bison data, we need to arrange months horizontally
# rather than vertically. Here is how to do it for the precip data:
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

train = subset(bison_wide, year < 2012)
test = subset(bison_wide, year >= 2012)

# fit weather model
m1 = lm(logN ~ loglagN + ppt_Jan, data=train )
summary(m1)

# extract estimates of parameter means and var-cov matrix
mu = coef(m1)
sigma = vcov(m1)
sigma_zero = matrix(0,3,3)

# get SD of process error
PE = sd(residuals(m1))

# make up initial conditions uncertainty
OE = 0.1

# Monte Carlo simulations
NE = 1000      # Ensemble size
tot_time = dim(test)[1] + 1
nClim = matrix(NA,NE,tot_time)   # storage for all simulations
init_obs = test$loglagN[test$year==2012]
nClim[,1] = rnorm(NE,init_obs,0)
coef_samples = rmvnorm(NE,mean=mu,sigma=sigma)  # sample values of coefficients
for(i in 1:NE){        # loop over coefficient samples
  for(j in 2:tot_time){
    best_guess = coef_samples[i,1] + coef_samples[i,2]*nClim[i,j-1] + coef_samples[i,3]*test$ppt_Jan[test$year==2010+j]   # predict values at each x
    nClim[i,j] = rnorm(1,best_guess,PE) # add process error
  }
}

# calculate the median and 95% CI limits for each time point
nClim.stats = apply(nClim,2,quantile,c(0.025,0.5,0.975))
nClim.stats = nClim.stats[,2:NCOL(nClim.stats)]

# plot the data
plot(c(train$year,test$year),c(train$logN,test$logN),ylim=c(6,10),type="l",xlab="Year",ylab="log N")
# add predictions, upper and lower CI's
lines(test$year,nClim.stats[2,],col="blue",lty="solid")
lines(test$year,nClim.stats[1,],col="blue",lty="dashed")
lines(test$year,nClim.stats[3,], col="blue",lty="dashed")

accuracy(nClim.stats[2,],test$logN)

