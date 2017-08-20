#count <- 1

# All forecasts

library(forecast)

# File Read
original <- read.csv(file="C:/FinancialData.csv",header=TRUE)

# All currency data
reqdata_sgd = original$SG.D
reqdata_pd = original$P.D
reqdata_sgp = original$SG.P

# Checking seasonality
fit <- tbats(original$SG.D)
seasonal <- !is.null(fit$seasonal)
seasonal

# Training Set 
rates_ts_sgd <- ts(reqdata_sgd, frequency = 52, start=c(1987,1))
rates_ts_sub_sgd <- ts(reqdata_sgd, frequency = 52, start=c(1987,1),end =c(1996,12))

rates_ts_pd <- ts(reqdata_pd, frequency = 52, start=c(1987,1))
rates_ts_sub_pd <- ts(reqdata_pd, frequency = 52, start=c(1987,1),end =c(1996,12))

rates_ts_sgp <- ts(reqdata_sgp, frequency = 52, start=c(1987,1))
rates_ts_sub_sgp <- ts(reqdata_sgp, frequency = 52, start=c(1987,1),end =c(1996,12))

# Forecast imputation variables
impute_forecast_sgd <- ts(reqdata_sgd, frequency = 52, start=c(1987,1),end =c(1996,12))
impute_forecast_pd <- ts(reqdata_pd, frequency = 52, start=c(1987,1),end =c(1996,12))
impute_forecast_sgp <- ts(reqdata_sgp, frequency = 52, start=c(1987,1),end =c(1996,12))

# FORECASTING SECTION
for (i in seq(481,596,1)){
  m_ets_sgd = auto.arima(rates_ts_sub_sgd)
  f_ets_sgd = forecast(m_ets_sgd, h=1)
  
  m_ets_pd = auto.arima(rates_ts_sub_pd)
  f_ets_pd = forecast(m_ets_pd, h=1)
  
  m_ets_sgp = auto.arima(rates_ts_sub_sgp)
  f_ets_sgp = forecast(m_ets_sgp, h=1)
  
  #forecastvec = c(forecastvec,f_ets$mean[1][1])
  # rates_ts_sub = rates_ts_sub[2:length(rates_ts_sub)]
  rates_ts_sub_sgd = c(rates_ts_sub_sgd, rates_ts_sgd[i])
  rates_ts_sub_pd = c(rates_ts_sub_pd, rates_ts_pd[i])
  rates_ts_sub_sgp = c(rates_ts_sub_sgp, rates_ts_sgp[i])
  # rates_ts_sub = (rates_ts_sub)
  #plot(f_ets)
  print(i)
  print(f_ets_sgd$mean[1])
  impute_forecast_sgd = c(impute_forecast_sgd,f_ets_sgd$mean[1])
  impute_forecast_pd = c(impute_forecast_pd,f_ets_pd$mean[1])
  impute_forecast_sgp = c(impute_forecast_sgp,f_ets_sgp$mean[1])
  
}

# PLOTTING
impute_forecast_sgd = ts(impute_forecast_sgd,frequency = 52, start=c(1987,1))
plot(impute_forecast_sgd,type="l")

impute_forecast_pd = ts(impute_forecast_pd,frequency = 52, start=c(1987,1))
plot(impute_forecast_pd,type="l")

impute_forecast_sgp = ts(impute_forecast_sgp,frequency = 52, start=c(1987,1))
plot(impute_forecast_sgp,type="l")

#lines(rates_ts[481:596],type="l",col="red")

compare_sgd = impute_forecast_sgd[481:596]-rates_ts_sgd[481:596]
plot(compare_sgd,type='l')

compare_pd = impute_forecast_pd[481:596]-rates_ts_pd[481:596]
plot(compare_pd,type='l')

compare_sgp = impute_forecast_sgp[481:596]-rates_ts_sgp[481:596]
plot(compare_sgp,type='l')

output_sgd <- data.frame(actual=rates_ts_sgd[481:596],pred = impute_forecast_sgd[481:596])
lines(output_sgd,type='l',col='blue')

output_pd <- data.frame(actual=rates_ts_pd[481:596],pred = impute_forecast_pd[481:596])
lines(output_pd,type='l',col='green')

output_sgp <- data.frame(actual=rates_ts_sgp[481:596],pred = impute_forecast_sgp[481:596])
lines(output_sgp,type='l',col='yellow')

# Deriving other fields
original$SG.D_fc = (impute_forecast_sgd)
original$P.D_fc = (impute_forecast_pd)
original$SG.P_fc = (impute_forecast_sgp)
original$D.SG_fc = (1/impute_forecast_sgd)
original$P.SG_fc = (1/impute_forecast_sgp)
original$D.P_fc = (1/impute_forecast_pd)

original$D.SG = (1/original$SG.D)
original$D.P = (1/original$P.D)
original$P.SG = (1/original$SG.P)

head(original)

#count <- count+1

write.csv(original,file="C:/FinancialData_Forecasted.csv")

#-----------------------------------------------------------------------------------
  

