# Forecast for SGD with ARIMA - Trial 1

library(forecast)
rates <- read.csv(file="C:/ARIMAData.csv",header=TRUE)
data = rates$SG_D

#data = log(data)

ts.data = ts(data, frequency = 52, start = c(1987,1))
plot(ts.data)

# Training and testing sets

data.train = window(ts.data,start = c(1987,1), end = c(1994,52))
plot(data.train)

data.test = window(ts.data,start = c(1995,1), end = c(1995,52))
plot(data.test)

# Running the arima
# 
arima1 = auto.arima(data.train, trace = TRUE, test="kpss", ic = "aic")
summary(arima1)
confint(arima1)

plot.ts(arima1$residuals)
acf(arima1$residuals,main="ACF")

arima1.forecast = forecast.Arima(arima1,h=52)
arima1.forecast
plot(arima1.forecast)
