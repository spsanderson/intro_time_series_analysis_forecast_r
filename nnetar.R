## Import the APTelectricity.csv file as APTelectric

myts = ts(APTelectric$watt, frequency = 288)

plot(myts)

set.seed(34)

library(forecast)
fit = nnetar(myts)
nnetforecast <- forecast(fit, h = 400, PI = F)

library(ggplot2)
autoplot(nnetforecast)

## Using an external regressor in a neural net
fit2 = nnetar(myts, xreg = APTelectric$appliances)

# Defining the vector which we want to forecast
y =rep(2, times = 12*10)

nnetforecast <- forecast(fit2, xreg = y, PI = F)

library(ggplot2)
autoplot(nnetforecast)