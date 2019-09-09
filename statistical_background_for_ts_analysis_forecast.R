lynx
time(lynx)
length(lynx)
tail(lynx) # last 6 observations
mean(lynx); median(lynx)
plot(lynx)
sort(lynx)
sort(lynx)[c(57,58)]
quantile(lynx)
quantile(lynx, prob = seq(0, 1, length = 11), type = 5)

### simple forecast methods
set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

plot(meanm, plot.conf = F, main = "")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("topleft",lty=1,col=c(4,123,22),
       legend=c("Mean method","Naive method","Drift Method"))

###### accuracy and model comparison
set.seed(95)
myts <- ts(rnorm(200), start = (1818))
mytstrain <- window(myts, start = 1818, end = 1988)
plot(mytstrain)

library(forecast)
meanm <- meanf(mytstrain, h=30)
naivem <- naive(mytstrain, h=30)
driftm <- rwf(mytstrain, h=30, drift = T)

mytstest <- window(myts, start = 1988)

accuracy(meanm, mytstest)
accuracy(naivem, mytstest)
accuracy(driftm, mytstest)

###### Residuals

set.seed(95)
myts <- ts(rnorm(200), start = (1818))
plot(myts)

library(forecast)
meanm <- meanf(myts, h=20)
naivem <- naive(myts, h=20)
driftm <- rwf(myts, h=20, drift = T)

var(meanm$residuals)
mean(meanm$residuals)

mean(naivem$residuals)

naivwithoutNA <- naivem$residuals
naivwithoutNA <- naivwithoutNA[2:200]
var(naivwithoutNA)
mean(naivwithoutNA)

driftwithoutNA <- driftm$residuals
driftwithoutNA <- driftwithoutNA[2:200]
var(driftwithoutNA)
mean(driftwithoutNA)

hist(driftm$residuals)

acf(driftwithoutNA)

### Stationarity
x <- rnorm(1000) # no unit-root, stationary

library(tseries)
adf.test(x) # augmented Dickey Fuller Test
plot(nottem) # Let s see the nottem dataset
plot(decompose(nottem))
adf.test(nottem)

y <- diffinv(x) # non-stationary
plot(y)
adf.test(y)

### Autocorrelation
# Durbin Watson test for autocorrelation
length(lynx); head(lynx); head(lynx[-1]); head(lynx[-114]) # check the required traits for the test

library(lmtest)
dwtest(lynx[-114] ~ lynx[-1])
x = rnorm(700) # Lets take a look at random numbers
dwtest(x[-700] ~ x[-1])
length(nottem) # and the nottem dataset
dwtest(nottem[-240] ~ nottem[-1])

### ACF and PACF
acf(lynx, lag.max = 20); pacf(lynx, lag.max =20, plot = F)
# lag.max for numbers of lags to be calculated
# plot = F to suppress plotting
acf(rnorm(500), lag.max = 20)

## Exercise messy data
set.seed(54)
myts <- ts(c(rnorm(50, 34, 10), 
             rnorm(67, 7, 1), 
             runif(23, 3, 14)))

# myts <- log(myts)
plot(myts)

library(forecast)
meanm <- meanf(myts, h=10)
naivem <- naive(myts, h=10)
driftm <- rwf(myts, h=10, drift = T)

plot(meanm, main = "", bty = "l")
lines(naivem$mean, col=123, lwd = 2)
lines(driftm$mean, col=22, lwd = 2)
legend("bottomleft",lty=1,col=c(4,123,22), bty = "n", cex = 0.75,
       legend=c("Mean method","Naive method","Drift Method"))

length(myts)
mytstrain <- window(myts, start = 1, end = 112 )
mytstest <- window(myts, start = 113)

meanma <- meanf(mytstrain, h=28)
naivema <- naive(mytstrain, h=28)
driftma <- rwf(mytstrain, h=28, drift = T)

accuracy(meanma, mytstest)
accuracy(naivema, mytstest)
accuracy(driftma, mytstest)

plot(naivem$residuals)

mean(naivem$residuals[2:140])

hist(naivem$residuals) # normal distribution

shapiro.test(naivem$residuals) # test for normal distribution, normal distr can be rejected

acf(naivem$residuals[2:140]) # autocorrelation test, autocorrelation present