# Currencies we have
t_rates <- read.csv(file="C:\\predict.csv",header=TRUE)

# Initial Values
US_Account = 10000
SGD_Account = 10000
UKP_Account = 10000

# Exchange Rates
sgd_Forex = t_rates$SG.D
dsg_Forex = t_rates$D.SG
pd_Forex = t_rates$P.D
dp_Forex = t_rates$D.P
sgp_Forex = t_rates$SG.P
psg_Forex = t_rates$P.SG

# Number of Records
n_weeks = dim(t_rates)[1]

# Difference between Forecast for next week and this weeks exchange rate
sgd_Diff = t_rates$Diff.SG.D
pd_Diff = t_rates$Diff.P.D
sgp_Diff = t_rates$Diff.SG.P

# Percent of principal traded
percent_traded = 0.003

# Prime rates
USRates = t_rates$USPRIME
UKRates = t_rates$UKPRIME
SRates = t_rates$SGPRIME

Total_Prof = 0

# Weekly Trades
for (i in 1:n_weeks){
  US_Account_Previous = US_Account
  SGD_Account_Previous = SGD_Account
  UKP_Account_Previous = UKP_Account
  
  # SG.D and D.SG
  if (sgd_Diff[i]>0){ # SG/D projected to go up, trade to D
    SGD_Traded = percent_traded * SGD_Account 
    SGD_Debited = SGD_Traded + (0.01 * SGD_Traded) # 1% debited for trade 
    SGD_Account =  SGD_Account - SGD_Debited
    
    US_Credited = SGD_Traded * sgd_Forex[i]
    US_Account = US_Account + US_Credited
  }
  else{
    US_Traded = percent_traded * US_Account
    US_Debited = US_Traded + (0.01 * US_Traded) 
    US_Account =  US_Account - US_Debited
    
    SGD_Credited = US_Traded * dsg_Forex[i]
    SGD_Account = SGD_Account + SGD_Credited
  }
  
  # P.D and D.P
  if (pd_Diff[i]>0){ #if P/D projected to go up, trade to D  
    UKP_Traded = percent_traded * UKP_Account 
    UKP_Debited = UKP_Traded + (0.01 * UKP_Traded) 
    UKP_Account =  UKP_Account - UKP_Debited
    
    US_Credited = UKP_Traded * pd_Forex[i]
    US_Account = US_Account + US_Credited
  }
  else{
    US_Traded = percent_traded * US_Account
    US_Debited = US_Traded + (0.01 * US_Traded) 
    US_Account =  US_Account - US_Debited
    
    UKP_Credited = US_Traded * dp_Forex[i]
    UKP_Account = UKP_Account + UKP_Credited
  }
  
  # SG.P and P.SG
  if (sgp_Diff[i]>0){ # SG/P projected to go up, trade to P
    SGD_Traded = percent_traded * SGD_Account 
    SGD_Debited = SGD_Traded + (0.01 * SGD_Traded) # 1% debited for trade 
    SGD_Account =  SGD_Account - SGD_Debited
    
    UKP_Credited = SGD_Traded * sgp_Forex[i]
    UKP_Account = UKP_Account + UKP_Credited
  }
  else{
    UKP_Traded = percent_traded * UKP_Account
    UKP_Debited = UKP_Traded + (0.01 * UKP_Traded) 
    UKP_Account =  UKP_Account - UKP_Debited
    
    SGD_Credited = UKP_Traded * psg_Forex[i]
    SGD_Account = SGD_Account + SGD_Credited
  }
  
  # updating the weekly final amount in each account on basis of interest rates and profit
  US_Account = US_Account*(1+((USRates[i]/(100*52)))^((7/365)*52))
  USProf = US_Account-US_Account_Previous
  
  UKP_Account = UKP_Account*(1+((UKRates[i]/(100*52)))^((7/365)*52))
  UKProf = (UKP_Account-UKP_Account_Previous)*pd_Forex[i]
  
  SGD_Account = SGD_Account*(1+((SRates[i]/(100*52)))^((7/365)*52))
  SProf = (SGD_Account-SGD_Account_Previous)*sgd_Forex[i]
  
  # Total Profit calculated on a daily basis
  Total_Prof = Total_Prof + (USProf+UKProf+SProf)
}

cat("Total US Dollars = ", US_Account,"$")
cat("Total SG Dollars = ", SGD_Account,"$")
cat("Total UK Pounds = ", UKP_Account,"£")
cat("Total Profit in US Dollars = ", Total_Prof,"$")
