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

library(mice)
md.pattern(ts_NAandOutliers)
df_miced <- mice(
  ts_NAandOutliers
  , m = 10
  , method = 'rf'
)
class(df_miced)
df_miced$imp$mydata
df <- complete(df_miced, 5)
summary(df)

library(outliers)
dixon.test(df$mydata)