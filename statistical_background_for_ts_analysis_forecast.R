# Statistical Background for Time Series Anaysis and Forecasting

# Lynx Data ###
lynx
time(lynx)
length(lynx)
tail(lynx)
mean(lynx)
median(lynx)
plot(lynx)

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h = 20)
naivem <- naive(myts, h = 20)
driftm <- rwf(myts, h = 20)

plot(meanm, main = "")
lines(naivem$mean, col = 123, lwd = 2)
lines(driftm$mean, col = 22, lwd = 2)
legend("topleft", lty = 1, col = c(4, 123, 22), legend = c("Mean Method", "Naive Method", "Drift Method"))

# Forecast acc