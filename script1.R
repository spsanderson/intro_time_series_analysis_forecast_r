# Lib Load ####
install.load::install_load(
  "tidyverse"
  , "forecast"
)

# TS Object ####
mycounts <- ts(
  my_ts$X2
  , start = 1
  , frequency = 12
)

# Viz ####
plot(
  mycounts
  , ylab = 'Customer Counts'
  , xlab = 'Weeks'
)

monthplot(
  mycounts
  , labels = 1:12
  , xlab = "Bidaily Units"
)
seasonplot(
  mycounts
  , season.labels = F
  , xlab = ""
)

# Model ####
plot(forecast(auto.arima(mycounts)))
