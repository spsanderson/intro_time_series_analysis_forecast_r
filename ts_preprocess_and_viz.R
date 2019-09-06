# season plot airpassengers
library(forecast)
data("AirPassengers")
head(AirPassengers)

ggseasonplot(AirPassengers)

seasonplot(
  AirPassengers
  , xlab = ""
  , col = c("red","blue")
  , year.labels = T
  , labelgap = 0.75
  , type = "l"
  , bty = "l"
  , cex = 0.75
  )
 
# Irregular data ####
library(readr)
irregular_sensor <- read_csv(
  "irregular_sensor.csv"
  , col_names = FALSE
  , col_types = cols(
    X1 = col_character()
    , X2 = col_double()
    )
  )
View(irregular_sensor)
str(irregular_sensor)

library(zoo)
library(tidyr)

# Data is irregular so we aggregate by day since there is at least one measurement per day
# POSIXct/lt Date
# convert
# aggregate
# ts()
irregular_sensor_split = separate(
  irregular_sensor
  , col = X1
  , into = c("date","time")
  , sep = 8
  , remove = T
)

sensor_date <- strptime(irregular_sensor_split$date, '%m/%d/%y')
irregts_df <- data.frame(
  date = as.Date(sensor_date)
  , measurement = irregular_sensor_split$X2
)

irregts_ts <- zoo(
  irregts_df$measurement
  , order.by = irregts_df$date
)

# regularize data
irregts_ts <- aggregate(
  irregts_ts
  , as.Date
  , mean
)
irregts_ts
plot.ts(irregts_ts)

# keep date and time
sensonr.date1 <- strptime(
  irregular_sensor$X1
  , "%m/%d/%y %I:%M %p"
)
sensonr.date1

# create zoo object
irreg.dates1 <- zoo(
  irregular_sensor$X2
  , order.by = sensonr.date1
)
irreg.dates1
plot(irreg.dates1)

ag.irregtime1 <- aggregate(
  irreg.dates1
  , as.Date
  , mean
)
ag.irregtime1
plot.ts(ag.irregtime1)
myts <- ts(ag.irregtime1)
plot(myts)

# Missing & outliers ####
library(readr)
ts_NAandOutliers <- read_csv(
  "ts_NAandOutliers.csv"
  , col_types = cols(X1 = col_integer()))
View(ts_NAandOutliers)

dfts <- ts(ts_NAandOutliers$mydata)
dfts

summary(dfts)
plot.ts(dfts)

# Auto outlier detection
library(forecast)
dfts1 <- tsoutliers(dfts)
dfts1

# Missing values NA
# Missing data imputation or replacement
library(zoo)
myts.NAlocf <- na.locf(dfts)
myts.NAlocf

myts.NAfill <- na.fill(dfts, 33)
myts.NAfill

# na.trim 
myts.natrim <- na.trim(dfts, sides = "both")
myts.natrim

# na.interp
myts.nainterp <- na.interp(dfts)
myts.nainterp

# tsclean() much quicker and easier
myts.clean <- tsclean(dfts)
myts.clean
plot.ts(myts.clean)
summary(myts.clean)

# Course TSPP script
## U Standard time series functions
# Lets create a time series object - class ts
# Getting data
mydata = runif(n = 50, min = 10, max = 45)

# ts for class time series
# Data starts in 1956 - 4 observations/year (quarterly)
mytimeseries = ts(data = mydata, 
                  start = 1956, frequency = 4)

# Lets see how the data looks
plot(mytimeseries)

# Checking the class
class(mytimeseries)

# Checking the timestamp
time(mytimeseries)

# Refining the start argument
mytimeseries = ts(data = mydata, 
                  start = c(1956,3), frequency = 4)

## Creating a ts object - Exercise
# Get a random walk of 450 numbers, eg rnorm, runif, etc 
# In the solution I am going to use a cumulative sum on the normal distribution x = cumsum(rnorm(n = 450)) 
# If you want it to be reproducible, you can set a seed 
# Add the time component: it is a monthly dataset, which starts in November 1914 
# Get a simple plot for this time series # Advanced: how would you get the same type with the "lattice" package?
x = cumsum(rnorm(n = 450)) 
y = ts(x, start = c(1914,11), frequency = 12)
plot(y)
library(lattice)
xyplot.ts(y)

## U Plots for time series data
# Standard R Base plots
plot(nottem) 

# Plot of components
plot(decompose(nottem)) 

# Directly plotting a forecast of a model
plot(forecast(auto.arima(nottem)), h = 5)

# Random walk
plot.ts(cumsum(rnorm(500)))

# Add on packages for advanced plots
library(forecast)
library(ggplot2)

# The ggplot equivalent to plot
autoplot((nottem))

# Ggplots work with different layers
autoplot(nottem) + ggtitle("Autoplot of Nottingham temperature data")

# Time series specific plots
ggseasonplot(nottem) 
ggmonthplot(nottem)

## Exercise Seasonplot - library (forecast)
# use the seasonplot function in order  
# to resemble the plot shown here 
# we are using the AirPassengers dataset  
# for all the needed arguments on the plot, 
# check out the help for par 
# make sure that the labels are visible
library(forecast) 
seasonplot(AirPassengers, xlab = "", col = c("red", "blue"), year.labels = T, labelgap = 0.35, type = "s",  bty = "7", cex = 0.75,  main = "Seasonal plot of dataset AirPassengers")

## German Inflation Rates
https://www.statbureau.org/en/germany/inflation-tables

# data is copied from web, first and last col deleted
mydata = scan()
plot.ts(mydata)
germaninfl = ts(mydata, start = 2008, frequency = 12)
plot(germaninfl)

### Working with Irregular Time Series
## dataset: irregular_sensor

# Irregular_sensor time series csv
class(irregular_sensor$V1) 

library(zoo) # for general irregular time series
library(tidyr) # for function separate

## Method 1 - removing the time component
irreg.split = separate(irregular_sensor, col = V1, 
                       into = c('date', 'time'),
                       sep = 8, remove = T)

# Using only the date
sensor.date = strptime(irreg.split$date, '%m/%d/%y')

# Creating a data.frame for orientation
irregts.df = data.frame(date = as.Date(sensor.date), 
                        measurement = irregular_sensor$V2)

# Getting a zoo object
irreg.dates = zoo(irregts.df$measurement,
                  order.by = irregts.df$date)

# Regularizing with aggregate
ag.irregtime = aggregate(irreg.dates, 
                         as.Date, mean)

ag.irregtime
length(ag.irregtime)

####
## Method 2 - date and time component kept
sensor.date1 = strptime(irregular_sensor$V1,
                        '%m/%d/%y %I:%M %p')
sensor.date1

# Creating the zoo object
irreg.dates1 = zoo(irregular_sensor$V2, 
                   order.by = sensor.date1)
irreg.dates1

plot(irreg.dates1)

# Regularizing with aggregate
ag.irregtime1 = aggregate(irreg.dates1, 
                          as.Date, mean)

ag.irregtime1
plot(ag.irregtime1) # plotting the regular zoo object directly
myts = ts(ag.irregtime1) # converting to a standard ts, the days start at 1
plot(myts)

### Working with Missing Data and Outliers 
## Import ts.NAandOutliers.csv

# Convert the 2nd column to a simple ts without frequency
myts = ts(mydata$mydata)
myts

# Checking for NAs and outliers
summary(myts)
plot(myts)

# Automatic detection of outliers
library(forecast)
myts1 = tsoutliers(myts)
myts1
plot(myts)

# Missing data handling with zoo
library(zoo)
myts.NAlocf = na.locf(myts)

myts.NAfill = na.fill(myts, 33)

# Tip: na.trim to get rid of NAs at the beginning or end of dataset

# Standard NA method in package forecast
myts.NAinterp = na.interp(myts)

# Cleaning NA and outliers with forecast package
mytsclean = tsclean(myts)
plot(mytsclean)
summary(mytsclean)