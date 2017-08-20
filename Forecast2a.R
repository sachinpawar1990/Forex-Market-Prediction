library(forecast)
rates <- read.csv(file="C:/Users/sachi/Dropbox/Project/Book1.csv",header=TRUE)
head(rates)
str(rates)
rates$Date <- as.Date(rates$Date, "%m-%d-%Y")
range(rates$Date)
str(rates$Date)
rates <- rates[order(rates$Date), ]
plot(rates$Date,rates$SG.D,type="l")
head(rates$Date, 20)
head(rates$Date, 20)
years <- format(rates$Date, "%Y")
head(years)
tab <- table(years)
tab
mean(tab[1:(length(tab) - 1)])

## Forecast with ARIMA model
forecastArima <- function(x, n.ahead){
  myTs <- ts(rates$SG.D, frequency=52, start=c(1987,1))
  #fit.arima <- auto.arima(myTs)
  fit.arima <- arima(myTs, order=c(0,1,0))
  fore <- forecast(fit.arima, h=30)
  plot(fore)
  upper <- fore$upper[,'95%']
  lower <- fore$lower[,'95%']
  trend <- as.numeric(fore$fitted)
  pred <- as.numeric(fore$mean)
  output <- data.frame(actual = c(x$SG.D, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))  
  )
  return(output)
}

plotForecastResult <- function(x, title=NULL) {
  x <- x[order(x$date),]
  max.val <- max(c(x$actual, x$upper), na.rm=T)
  min.val <- min(c(x$actual, x$lower), na.rm=T)
  plot(x$date, x$actual, type="l", col="grey", main=title,
       xlab="Time", ylab="Exchange Rate",
       xlim=range(x$date), ylim=c(min.val, max.val))
  grid()
  lines(x$date, x$trend, col="yellowgreen")
  lines(x$date, x$pred, col="green")
  lines(x$date, x$lower, col="blue")
  lines(x$date, x$upper, col="blue")
  legend("bottomleft", col=c("grey", "yellowgreen", "green", "blue"), lty=1,
         c("Actual", "Trend", "Forecast", "Lower/Upper Bound"))
}



forecastStl <- function(x, n.ahead=30){
  myTs <- ts(x$SG.D, start=1, frequency=52)
  fit.stl <- stl(myTs, s.window=52)
  sts <- fit.stl$time.series
  trend <- sts[,"trend"]
  fore <- forecast(fit.stl, h=n.ahead, level=95)
  plot(fore)
  pred <- fore$mean
  upper <- fore$upper
  lower <- fore$lower
  output <- data.frame(actual = c(x$SG.D, rep(NA, n.ahead)),
                       trend = c(trend, rep(NA, n.ahead)),
                       #pred = c(trend, pred),
                       pred = c(rep(NA, nrow(x)), pred),
                       lower = c(rep(NA, nrow(x)), lower),                       
                       upper = c(rep(NA, nrow(x)), upper),                       
                       date = c(x$Date, max(x$Date) + (1:n.ahead))
  )
  return(output)
}




result.arima <- forecastArima(rates, n.ahead = 52)
plotForecastResult(result.arima, title = "Exchange rate forecasting with ARIMA")

#result.stl <- forecastStl(rates, n.ahead = 700)
#plotForecastResult(result.stl, title = "Exchange rate forecasting with STL")
