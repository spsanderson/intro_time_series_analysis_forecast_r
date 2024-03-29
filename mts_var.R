### Multivariate Time Series Datasets
# Generating a random dataframe
x = rnorm(100, 1)
y = rnorm(100, 30)
z = rnorm(100, 500)

xyz = data.frame(x, y, z)

class(xyz)

# Converting a data.frame into mts
mymts = ts(xyz,
           frequency = 12,
           start = c(1940, 4))

# Standard exploratory tools
plot(mymts)
head(mymts)
class(mymts)

# Our further exercise dataset
class(EuStockMarkets)
head(EuStockMarkets)

# Main packages - problem: both have different functions VAR
library(vars)
library(MTS)

## Testing for stationarity
library(tseries)

### tseries - standard test adt.test
apply(EuStockMarkets, 2, adf.test)

# Alternative: lib fUnitRoots, function
apply(EuStockMarkets, 2, adf.test, lags=0, type="c")

# Differencing the whole mts
library(MTS)

stnry = diffM(EuStockMarkets)

# Retest
apply(stnry, 2, adf.test)

## VAR modeling
plot.ts(stnry)

# Lag order identification
library(vars)

VARselect(stnry, type = "none", lag.max = 10)

# Creating a VAR model with vars
var.a <- vars::VAR(stnry,
                   lag.max = 10,
                   ic = "AIC",
                   type = "none")

summary(var.a)

# Residual diagnostics
serial.test(var.a)

# Granger test for causality
causality(var.a, cause = c("DAX"))

## Forecasting VAR models
fcast = predict(var.a, n.ahead = 25)
plot(fcast)

# Forecasting the DAX index
DAX = fcast$fcst[1]; DAX # type list

# Extracting the forecast column
x = DAX$DAX[,1]; x

tail(EuStockMarkets)

# Inverting the differencing
x = cumsum(x) + 5473.72
plot.ts(x)

# Adding data and forecast to one time series
DAXinv =ts(c(EuStockMarkets[,1], x),
           start = c(1991,130), frequency = 260)
plot(DAXinv)
plot.ts(DAXinv[1786:1885])

## Creating an advanced plot with visual separation
library(lattice)
library(grid)
library(zoo)

# Converting to object zoo
x = zoo(DAXinv[1786:1885])

# Advanced xyplot from lattice
xyplot(x, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(76, "native"), just=c("right"))
  panel.xyplot(x, y, col="green", ...) })
