

# start with a clean slate
rm(list=ls())

# it's good coding practice to load packages at the top of a script
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)

# set working directory to ensure R can find the file we wish to import
setwd("C:/Repos/forecasting-dynamics-course/lectures")  # your path will differ!

###
### Tutorial 02 Dealing with dates and times ------------------------------------------
###

# Load csv file of 15 min meteorological data from Harvard Forest
# Factors=FALSE so strings, series of letters/words/numerals, remain characters
harMet_15Min <- read.csv(
  file="./../data/HARV/FisherTower-Met/hf001-10-15min-m.csv", # your path may differ!
  stringsAsFactors = FALSE)

# view column data class
class(harMet_15Min$datetime)

# view sample data
head(harMet_15Min$datetime)

# Convert character data to date (no time) 
myDate <- as.Date("2015-10-19 10:15")   
str(myDate)

# what happens if the date has text at the end?
myDate2 <- as.Date("2015-10-19Hello")   
str(myDate2)

# Convert character data to date and time.
timeDate <- as.POSIXct("2015-10-19 10:15")   
str(timeDate)

timeDate

# to see the data in this 'raw' format, i.e., not formatted according to the 
# class type to show us a date we recognize, use the `unclass()` function.
unclass(timeDate)

# Convert character data to POSIXlt date and time
timeDatelt<- as.POSIXlt("2015-10-19 10:15")  
str(timeDatelt)

timeDatelt

unclass(timeDatelt)

# view one date-time field
harMet_15Min$datetime[1]

# convert single instance of date/time in format year-month-day hour:min:sec
as.POSIXct(harMet_15Min$datetime[1],format="%Y-%m-%dT%H:%M")


## The format of date-time MUST match the specified format or the data will not
# convert; see what happens when you try it a different way or without the "T"
# specified
as.POSIXct(harMet_15Min$datetime[1],format="%d-%m-%Y%H:%M")

as.POSIXct(harMet_15Min$datetime[1],format="%Y-%m-%d%H:%M")

new.date.time <- as.POSIXct(harMet_15Min$datetime,
                            format="%Y-%m-%dT%H:%M" #format time
                            )

# view output
head(new.date.time)

# what class is the output
class(new.date.time)

# assign time zone to just the first entry
as.POSIXct(harMet_15Min$datetime[1],
            format = "%Y-%m-%dT%H:%M",
            tz = "America/New_York")

# convert to POSIXct date-time class
harMet_15Min$datetime <- as.POSIXct(harMet_15Min$datetime,
                                format = "%Y-%m-%dT%H:%M",
                                tz = "America/New_York")

# view structure and time zone of the newly defined datetime column
str(harMet_15Min$datetime)

tz(harMet_15Min$datetime)


###
### Tutorial 04 subset and manipulate data ------------------------------------------
###

rm(list=ls())

# 15-min Harvard Forest met data, 2009-2011
harMet15.09.11<- read.csv(
  file="./../data/HARV/FisherTower-Met/Met_HARV_15min_2009_2011.csv",
  stringsAsFactors = FALSE
  )

# convert datetime to POSIXct
harMet15.09.11$datetime<-as.POSIXct(harMet15.09.11$datetime,
                    format = "%Y-%m-%d %H:%M",
                    tz = "America/New_York")

##  15-min-plots, echo=FALSE

a <- ggplot(harMet15.09.11, aes(x=datetime, y=airt)) + 
           geom_point(na.rm=TRUE, size = .1) +
           scale_x_datetime(breaks=date_breaks("1 year")) +
           ggtitle("Air Temp \n NEON Harvard Forest Field Site") +
           xlab("Date") + 
           ylab("Air Temperature, Celcius")

b <- ggplot(harMet15.09.11, aes(x=datetime, y=prec)) +
          geom_point(na.rm=TRUE, size = .1) +
          scale_x_datetime(breaks=date_breaks("1 year")) +
           ggtitle("Precipitation \n NEON Harvard Forest Field Site")+
           xlab("Date") + ylab("Daily Total Precip., mm")

c <- ggplot(harMet15.09.11, aes(x=datetime, y=parr))+
          geom_point(na.rm=TRUE, size = .1) +
           scale_x_datetime(breaks=date_breaks("1 year")) +
           ggtitle("PAR \n NEON Harvard Forest Field Site") +
           xlab("Date") + ylab("Total PAR-Daily Mean")

grid.arrange(a,b,c, ncol=2)


## dplyr-lubridate-2
# create a year column
harMet15.09.11$year <- year(harMet15.09.11$datetime)

## dplyr-lubridate-3

# check to make sure it worked
names(harMet15.09.11)
str(harMet15.09.11$year)

## group-by-dplyr

# Create a group_by object using the year column 
HARV.grp.year <- group_by(harMet15.09.11, # data_frame object
                          year) # column name to group by

# view class of the grouped object
class(HARV.grp.year)

## tally-by-year
# how many measurements were made each year?
tally(HARV.grp.year)

# what is the mean airt value per year?
summarize(HARV.grp.year, 
          mean(airt)   # calculate the annual mean of airt
          ) 


## check-data
# are there NoData values?
sum(is.na(HARV.grp.year$airt))

# where are the no data values
# just view the first 6 columns of data
HARV.grp.year[is.na(HARV.grp.year$airt),1:6]


## calculate-mean-value
# calculate mean but remove NA values
summarize(HARV.grp.year, 
          mean(airt, na.rm = TRUE)
          )


## using-pipes

# how many measurements were made a year?
harMet15.09.11 %>% 
  group_by(year) %>%  # group by year
  tally() # count measurements per year


## summ-data
# what was the annual air temperature average 
year.sum <- harMet15.09.11 %>% 
  group_by(year) %>%  # group by year
  summarize(mean(airt, na.rm=TRUE))

# what is the class of the output?
year.sum
# view structure of output
str(year.sum)

## pipe-demo, echo = FALSE

# create dataframe
jday.avg <- harMet15.09.11 %>%      # within the harMet15.09.11 data
            group_by(jd) %>%      # group the data by the Julian day
            summarize((mean(airt,na.rm=TRUE)))  # summarize temp per julian day
names(jday.avg) <- c("jday","meanAirTemp")

# plot average air temperature by Julian day
qplot(jday.avg$jday, jday.avg$meanAirTemp,
        main="Average Air Temperature by Julian Day\n 2009-2011\n NEON Harvard Forest Field Site",
      xlab="Julian Day", ylab="Temp (C)")


## dplyr-group
harMet15.09.11 %>%         # use the harMet15.09.11 data_frame
  group_by(year, jd) %>%   # group data by Year & Julian day
  tally()                  # tally (count) observations per jd / year

## simple-math
24*4  # 24 hours/day * 4 15-min data points/hour

## dplyr-summarize
harMet15.09.11 %>%         # use the harMet15.09.11 data_frame
  group_by(year, jd) %>%   # group data by Year & Julian day
  summarize(mean_airt = mean(airt, na.rm = TRUE))  # mean airtemp per jd / year

## challenge-answer, echo=FALSE
# calculate total percip by year & day
total.prec <- harMet15.09.11 %>%
  group_by(year, jd) %>%
  summarize(sum_prec = sum(prec, na.rm = TRUE)) 

# plot precip
qplot(total.prec$jd, total.prec$sum_prec,
      main="Total Precipitation",
      xlab="Julian Day", ylab="Precip (mm)", 
      colour=as.factor(total.prec$year))


## dplyr-mutate

harMet15.09.11 %>%
  mutate(year2 = year(datetime)) %>%
  group_by(year2, jd) %>%
  summarize(mean_airt = mean(airt, na.rm = TRUE))


## dplyr-create-data-frame

harTemp.daily.09.11<-harMet15.09.11 %>%
                    mutate(year2 = year(datetime)) %>%
                    group_by(year2, jd) %>%
                    summarize(mean_airt = mean(airt, na.rm = TRUE))

head(harTemp.daily.09.11)

## dplyr-dataframe
# add in a datatime column
harTemp.daily.09.11 <- harMet15.09.11 %>%
  mutate(year3 = year(datetime)) %>%
  group_by(year3, jd) %>%
  summarize(mean_airt = mean(airt, na.rm = TRUE), datetime = first(datetime))

# view str and head of data
str(harTemp.daily.09.11)
head(harTemp.daily.09.11)

## challenge-code-dplyr, results="hide", include=TRUE, echo=FALSE
# 1
total.prec2 <- harMet15.09.11 %>%
  group_by(year, jd) %>%
  summarize(sum_prec = sum(prec, na.rm = TRUE), datetime = first(datetime)) 

qplot(x=total.prec2$datetime, y=total.prec2$sum_prec,
    main="Total Daily Precipitation 2009-2011\nNEON Harvard Forest Field Site",
    xlab="Date (Daily Values)", ylab="Precip (mm)")

# p2
harTemp.monthly.09.11 <- harMet15.09.11 %>%
  mutate(month = month(datetime), year= year(datetime)) %>%
  group_by(month, year) %>%
  summarize(mean_airt = mean(airt, na.rm = TRUE), datetime = first(datetime))

qplot(harTemp.monthly.09.11$datetime, harTemp.monthly.09.11$mean_airt,
  main="Monthly Mean Air Temperature 2009-2011\nNEON Harvard Forest Field Site",
  xlab="Date (Month)", ylab="Air Temp (C)")

str(harTemp.monthly.09.11)

###
###  Tutorial 05 plotting with ggplot --------------------------------------------------------
###

rm(list=ls())

# daily HARV met data, 2009-2011
harMetDaily.09.11 <- read.csv(
  file="./../data/HARV/FisherTower-Met/Met_HARV_Daily_2009_2011.csv",
  stringsAsFactors = FALSE)

# covert date to Date class
harMetDaily.09.11$date <- as.Date(harMetDaily.09.11$date)

# monthly HARV temperature data, 2009-2011
harTemp.monthly.09.11<-read.csv(
  file="./../data//HARV/FisherTower-Met/Temp_HARV_Monthly_09_11.csv",
  stringsAsFactors=FALSE
  )

# convert datetime from chr to date class & rename date for clarification
harTemp.monthly.09.11$date <- as.Date(harTemp.monthly.09.11$datetime)

## qplot
# plot air temp
qplot(x=date, y=airt,
      data=harMetDaily.09.11, na.rm=TRUE,
      main="Air temperature Harvard Forest\n 2009-2011",
      xlab="Date", ylab="Temperature (°C)")

## basic-ggplot2
# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_point(na.rm=TRUE)


## basic-ggplot2-colors
# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_point(na.rm=TRUE, color="blue", size=3, pch=18)


## basic-ggplot2-labels
# plot Air Temperature Data across 2009-2011 using daily data
ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_point(na.rm=TRUE, color="blue", size=1) + 
           ggtitle("Air Temperature 2009-2011\n NEON Harvard Forest Field Site") +
           xlab("Date") + ylab("Air Temperature (C)")


## basic-ggplot2-labels-named
# plot Air Temperature Data across 2009-2011 using daily data
AirTempDaily <- ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_point(na.rm=TRUE, color="purple", size=1) + 
           ggtitle("Air Temperature\n 2009-2011\n NEON Harvard Forest") +
           xlab("Date") + ylab("Air Temperature (C)")

# render the plot
AirTempDaily


## format-x-axis-labels
# format x-axis: dates
AirTempDailyb <- AirTempDaily + 
  (scale_x_date(labels=date_format("%b %y")))

AirTempDailyb

## format-x-axis-label-ticks
# format x-axis: dates
AirTempDaily_6mo <- AirTempDaily + 
    (scale_x_date(breaks=date_breaks("6 months"),
      labels=date_format("%b %y")))

AirTempDaily_6mo

# format x-axis: dates
AirTempDaily_1y <- AirTempDaily + 
    (scale_x_date(breaks=date_breaks("1 year"),
      labels=date_format("%b %y")))

AirTempDaily_1y


## subset-ggplot-time

# Define Start and end times for the subset as R objects that are the time class
startTime <- as.Date("2011-01-01")
endTime <- as.Date("2012-01-01")

# create a start and end time R object
start.end <- c(startTime,endTime)
start.end

# View data for 2011 only
# We will replot the entire plot as the title has now changed.
AirTempDaily_2011 <- ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_point(na.rm=TRUE, color="purple", size=1) + 
           ggtitle("Air Temperature\n 2011\n NEON Harvard Forest") +
           xlab("Date") + ylab("Air Temperature (C)")+ 
           (scale_x_date(limits=start.end,
                             breaks=date_breaks("1 year"),
                             labels=date_format("%b %y")))

AirTempDaily_2011


## nice-font
# Apply a black and white stock ggplot theme
AirTempDaily_bw<-AirTempDaily_1y +
  theme_bw()

AirTempDaily_bw

## install-new-theme
# install additional themes
# install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)
AirTempDaily_economist<-AirTempDaily_1y +
  theme_economist()

AirTempDaily_economist

AirTempDaily_strata<-AirTempDaily_1y +
  theme_stata()

AirTempDaily_strata


## increase-font-size
# format x axis with dates
AirTempDaily_custom<-AirTempDaily_1y +
  # theme(plot.title) allows to format the Title seperately from other text
  theme(plot.title = element_text(lineheight=.8, face="bold",size = 20)) +
  # theme(text) will format all text that isn't specifically formatted elsewhere
  theme(text = element_text(size=18)) 

AirTempDaily_custom


## challenge-code-ggplot-precip, echo=FALSE
# plot precip
PrecipDaily <- ggplot(harMetDaily.09.11, aes(date, prec)) +
           geom_point() +
           ggtitle("Daily Precipitation Harvard Forest\n 2009-2011") +
            xlab("Date") + ylab("Precipitation (mm)") +
            scale_x_date(labels=date_format ("%m-%y"))+
           theme(plot.title = element_text(lineheight=.8, face="bold",
                 size = 20)) +
           theme(text = element_text(size=18))

PrecipDaily

## ggplot-geom_bar
# plot precip
PrecipDailyBarA <- ggplot(harMetDaily.09.11, aes(date, prec)) +
    geom_bar(stat="identity", na.rm = TRUE) +
    ggtitle("Daily Precipitation\n Harvard Forest") +
    xlab("Date") + ylab("Precipitation (mm)") +
    scale_x_date(labels=date_format ("%b %y"), breaks=date_breaks("1 year")) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
    theme(text = element_text(size=18))

PrecipDailyBarA

## ggplot-geom_bar-subset, results="hide", include=TRUE, echo=FALSE
# Define Start and end times for the subset as R objects that are the date class
startTime2 <- as.Date("2010-01-01")
endTime2 <- as.Date("2011-01-01")

# create a start and end times R object
start.end2 <- c(startTime2,endTime2)
start.end2

# plot of precipitation
# subset just the 2011 data by using scale_x_date(limits)
ggplot(harMetDaily.09.11, aes(date, prec)) +
    geom_bar(stat="identity", na.rm = TRUE) +
    ggtitle("Daily Precipitation\n 2010\n Harvard Forest") +
    xlab("") + ylab("Precipitation (mm)") +
    scale_x_date(labels=date_format ("%B"),
    								 breaks=date_breaks("4 months"), limits=start.end2) +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
    theme(text = element_text(size=18))


## ggplot-color
# specifying color by name
PrecipDailyBarB <- PrecipDailyBarA+
  geom_bar(stat="identity", colour="darkblue")

PrecipDailyBarB


## ggplot-geom_lines
AirTempDaily_line <- ggplot(harMetDaily.09.11, aes(date, airt)) +
           geom_line(na.rm=TRUE) +  
           ggtitle("Air Temperature Harvard Forest\n 2009-2011") +
           xlab("Date") + ylab("Air Temperature (C)") +
           scale_x_date(labels=date_format ("%b %y")) +
           theme(plot.title = element_text(lineheight=.8, face="bold", 
                                          size = 20)) +
           theme(text = element_text(size=18))

AirTempDaily_line

## challenge-code-geom_lines&points, echo=FALSE
AirTempDaily + geom_line(na.rm=TRUE) 

## ggplot-trend-line
# adding on a trend lin using loess
AirTempDaily_trend <- AirTempDaily + stat_smooth(colour="green")

AirTempDaily_trend

## challenge-code-linear-trend, echo=FALSE
ggplot(harMetDaily.09.11, aes(date, prec)) +
      geom_bar(stat="identity", colour="darkorchid4") + #dark orchid 4 = #68228B
      ggtitle("Daily Precipitation with Linear Trend\n Harvard Forest") +
      xlab("Date") + ylab("Precipitation (mm)") +
      scale_x_date(labels=date_format ("%b %y"))+
      theme(plot.title = element_text(lineheight=.8, face="italic", size = 20)) +
      theme(text = element_text(size=18))+
      stat_smooth(method="lm", colour="grey")

## plot-airtemp-Monthly, echo=FALSE
AirTempMonthly <- ggplot(harTemp.monthly.09.11, aes(date, mean_airt)) +
    geom_point() +
    ggtitle("Average Monthly Air Temperature\n 2009-2011\n NEON Harvard Forest") +
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 20)) +
    theme(text = element_text(size=18)) +
    xlab("Date") + ylab("Air Temperature (C)") +
    scale_x_date(labels=date_format ("%b%y"))

AirTempMonthly


## compare-precip
# note - be sure library(gridExtra) is loaded!
# stack plots in one column 
grid.arrange(AirTempDaily, AirTempMonthly, ncol=1)

## challenge-code-grid-arrange, echo=FALSE
grid.arrange(AirTempDaily, AirTempMonthly, ncol=2)




