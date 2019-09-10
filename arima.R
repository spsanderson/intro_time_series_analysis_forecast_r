### ARIMA models

## ARIMA with auto.arima

plot(lynx)

library(forecast)

tsdisplay(lynx) # autoregression?

auto.arima(lynx) # the basic version

auto.arima(lynx, trace = T)

# recommended setting
auto.arima(lynx, trace = T, 
           stepwise = F, 
           approximation = F)



## ARIMA calculations



# AR(2) model

myarima = arima(lynx, order = c(2,0,0))
myarima

tail(lynx)
residuals(myarima)

# Check the equation for AR(2)
(2657 - 1545.45)*1.147 - (1590 - 1545.45)*0.6 + 601.84

3396 - 1545.45

# MA(2) model
myarima = arima(lynx, order = c(0,0,2))
myarima

residuals(myarima)

# Check the equation for MA(2)
844.72*1.141 + 255.91*0.47 + 766.83 

3396 - 1545.37



## ARIMA time series simulations

set.seed(123) # for reproduction

# simulation, at least n of 1000 

asim <- arima.sim(model = list(order = c(1,0,1), ar = c(0.4), ma = c(0.3)), n = 1000) + 10

plot(asim)

library(zoo) plot(rollmean(asim, 50)) plot(rollmean(asim, 25))

# Stationarity library(tseries) adf.test(asim)

# Autocorrelation library(forecast) tsdisplay(asim)

auto.arima(asim, trace = T,  stepwise = F,   approximation = F)



## ARIMA parameter selection

library(tseries)

adf.test(lynx)

library(forecast)
tsdisplay(lynx)

# Arima from forecast package
myarima <- Arima(lynx, order = c(4,0,0))

checkresiduals(myarima)

# Example MA time series

set.seed(123) # for reproduction

# Simulation
myts <- arima.sim(model = list(order = c(0,0,2),
                               ma = c(0.3, 0.7)), n = 1000) + 10

adf.test(myts) # Stationarity

tsdisplay(myts) # Autocorrelation

# Arima 
myarima <- Arima(myts, order = c(0,0,3))

checkresiduals(myarima)

auto.arima(myts, trace = T, 
           stepwise = F, 
           approximation = F)





## ARIMA Forecasting

# Our model 
myarima <- auto.arima(lynx, 
                      stepwise = F, 
                      approximation = F)

# Forecast of 10 years
arimafore <- forecast(myarima, h = 10)

plot(arimafore)

# See the forecasted values
arimafore$mean

# Plot last observations and the forecast
plot(arimafore, xlim = c(1930, 1944))

# Ets for comparison
myets <- ets(lynx)

etsfore <- forecast(myets, h = 10)

# Comparison plot for 2 models
library(ggplot2)

autoplot(lynx) +
  forecast::autolayer(etsfore$mean, series = 'ETS model') +
  forecast::autolayer(arimafore$mean, series = 'ARIMA model') +
  xlab('year') + ylab('Lynx Trappings') + 
  guides(colour = guide_legend(title = 'Forecast Method')) +
  theme(legend.position = c(0.8, 0.8))



## ARIMA with Explanatory Variables - Dynamic Regression
## Importing the cyprinidae dataset

# Display the multivariate dataset
library(ggplot2)
ggplot(cyprinidae,
       aes(y = concentration, x = X1)) + 
  geom_point () +
  aes(colour = predator_presence)

# Convert the variables into time series
x = ts(cyprinidae$concentration)
y = cyprinidae$predator_presence

# Arima model creation
library(forecast)
mymodel = auto.arima(x, xreg = y, 
                     stepwise = F,
                     approximation = F)
mymodel

# Quick check of model quality
checkresiduals(mymodel)

# Expected predator presence at future 10 time points
y1 = c(T,T,F,F,F,F,T,F,T,F)

# Getting a forecast based on future predator presence
plot(forecast(mymodel, xreg = y1))
plot(forecast(mymodel, xreg = y1),
     xlim  = c(230,260))