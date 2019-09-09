---
title: "Forecasting tournament"
output:
  html_document: default
layout: post
---

### Let the games begin ###

Do you think you are a good forecaster? A bad one? There is only one way to find out:
a competition! We are going to try to forecast forage production in the annual
grasslands of California based on weather covariates. The idea is pretty simple: 

1. Each group downloads:
      + The [forage production response data](https://github.com/pbadler/forecasting-dynamics-course/blob/master/data/sanjoaquin_forage_train_set.csv) for years 1936-2013.
      + [Monthly weather covariates](https://github.com/pbadler/forecasting-dynamics-course/blob/master/data/tournament_prism_monthly.csv) that span the training data set 
and then continue for the 2014-2018 period.
2. Fit whatever kind of model you want, using any method you want, with 
the 1936-2013 training data. You CANNOT use covariates from any other source.
3. Once you have finalized your model, you will generate a forecast
for years 2014-2018 (using the covariates for those years). Since 
we are hindcasting, the covariates are known, and we can ignore uncertainty 
in the covariates which would be present if we were relying on climate projections.
4. Each group will also prepare a 5 minute presentation for the rest of the 
class describing their model, and the choices they made to arrive at that model.
5. Submit your forecast as a .csv file on Canvas. If you do not estimate uncertainty
(confidence intervals), your file will just have one column of five numbers. If you
can estimate uncertainty, your file will have three columns: 1) the point forecast
for each year, and the 2) upper and 3) lower confidence limits for each year. *Please also
submit your R script.*
6. I will evaluate forecast accuracy by comparing your predictions to the observed 
2014-2018 values using mean absolute error (MAE). 
7. The winning team will bask in eternal glory, and a cheap candy prize. An additional
prize will go to the team making the most accurate forecast that also quantifies 
forecast uncertainty.

### Formatting ###

To make it easy for me to calculate the accuracy of your forecast,
please follow these formatting guidelines. Create a data frame with
two to four columns with the following names:
Year, Forecast, LowerCI, UpperCI.
The Year column should contain the integers 2014 to 2018, in order. 
The "Forecast" column contains your point forecasts for each
year. The point forecasts should be in the original units! If you fit
on a transformed scale, please back transform. 
The last two (CI) columns are optional. If you do report confidence intervals,
please calculate the 95% intervals (if it is too late, don't worry about it, just
turn in what you have). Again, these should be on the same scale as the observations.

Write your data frame to a .csv file using the following
line of code, substituting in the name of your data frame and the filename
you want to use (your group number?):
```
write.csv(your_data_frame, your_file.csv, header=T)
```
Finally, upload the .csv file to Canvas.

### Background ###

As we've discussed, mechanistic knowledge can improve forecasts. The forage
data come from the [San Joaquin Experimental Range](https://www.fs.fed.us/psw/ef/san_joaquin/), 
in the Sierra Nevada foothills 
of California. Here is the citation for the data, which USDA NRCS and 
University of California Extension have generously made public for our use:

Dennis Dudley, USDA NRCS Rangeland Specialist, Madera County; Neil McDougald, UCCE Livestock, 
Range, and Natural Resources Advisor Emeritus, Madera County

This is an annual grassland,
so measuring aboveground annual production (forage production) is straightforward:
biomass is clipped to ground level, dried, and weighed. This is usually done in June 
at the end of the spring growing season. For more information about the factors
that determine productivity in these grasslands, see this
[report](http://sfrec.ucanr.edu/files/183301.pdf).

I downloaded the weather data from the [PRISM Explorer](http://www.prism.oregonstate.edu/explorer/). You can find some
metadata [here](https://github.com/pbadler/forecasting-dynamics-course/blob/master/data/tournament_prism_notes.txt). "ppt" refers to precipitation, "t" to temperature,
and "vpd" to vapor pressure deficit.


