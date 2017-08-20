#Reading the Differences and Bank Rates
dnbrates <- read.csv("F:/predict2.csv",header=TRUE)

#Initial Values of All Currencies
USVal=10000
UKVal=10000
SVal=10000
USProf=0
UKProf=0
SProf=0
Total_Prof=0
per=0.0277

#Prime Rates

USRates=dnbrates$USPRIME
UKRates=dnbrates$UKPRIME
SRates=dnbrates$SGPRIME

diff_sgd=dnbrates$Diff.SG.D
diff_pd=dnbrates$Diff.P.D
diff_sgp=dnbrates$Diff.SG.P
diff_dsg=dnbrates$Diff.D.SG
diff_dp=dnbrates$Diff.D.P
diff_psg=dnbrates$Diff.P.SG

sgd=dnbrates$SG.D
pd=dnbrates$P.D
sgp=dnbrates$SG.P
dsg=dnbrates$D.SG
dp=dnbrates$D.P
psg=dnbrates$P.SG

n_rates = dim(dnbrates)[1]
#Total_Prof=0

#for(i in 1:2)
for(i in 2:n_rates)
{
  trd=which.max(c(diff_sgd[i],diff_pd[i],diff_sgp[i],diff_dsg[i],diff_dp[i],diff_psg[i]))
  
  #Storing Initial values in Prev's
  USVal_Prev=USVal
  UKVal_Prev=UKVal
  SVal_Prev=SVal
  
  # This is actually the amount(each converted to US Dollars) which you would get 
  # if there is bank deposit 
  USInt = USVal*(1+((USRates[i]/(100*52)))^((7/365)*52))
  # cat("USInt",USInt)
  UKInt = UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
  # cat("UKInt",UKInt)
  SInt = SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))*sgd[i]
  # cat("SInt",SInt)
  
  if(trd==1)
  {	
    # x is trading amount
    x=per*SVal
  
    # This is the amount(each converted to US Dollars) which you would get
    # if there is trading
    us_sgd_trd=USVal + (x*sgd[i])
    
    #Here it is assumed that interest is calculated along with trading profit
    us_sgd_trd=us_sgd_trd*(1+((USRates[i]/(100*52)))^((7/365)*52))
    # cat("us_sgd_trd",us_sgd_trd)
    uk_sgd_trd=UKInt
    #uk_sgd_trd=uk_sgd_trd*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
    # cat("uk_sgd_trd",uk_sgd_trd)
  
    sg_sgd_trd=(SVal-x-(x*0.01))
    sg_sgd_trd=sg_sgd_trd*(1+((SRates[i]/(100*52)))^((7/365)*52))*sgd[i]
    cat("sg_sgd_trd",sg_sgd_trd)
    
    # cat("Interest Total",(USInt+UKInt+SInt))
    # cat("Trading Trans",(us_sgd_trd+uk_sgd_trd+sg_sgd_trd))
    # 
    if((USInt+UKInt+SInt)>=(us_sgd_trd+uk_sgd_trd+sg_sgd_trd))
    { #print(1)
      USVal=USInt
      UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
      SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
      
    } 
    else 
    {
      USVal=us_sgd_trd
      SVal=(SVal-x-(x*0.01))*(1+((SRates[i]/(100*52)))^((7/365)*52))
      UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
      
    }
  }
  
  
  
  else if(trd==2)
  {	# y is trading amount
    y=per*UKVal

    us_pd_trd=USVal + (y*pd[i])
    us_pd_trd=us_pd_trd*(1+((USRates[i]/(100*52)))^((7/365)*52))
    
    uk_pd_trd=(UKVal-y-(y*0.01))
    uk_pd_trd=uk_pd_trd*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
    
    sg_pd_trd=SInt
    
    if((USInt+UKInt+SInt)>=(us_pd_trd+uk_pd_trd+sg_pd_trd))
    { #print(2)
      USVal=USInt
      UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
      SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
      
      
    } 
    else 
      { USVal=us_pd_trd
        UKVal=(UKVal-y-(y*0.01))*(1+((UKRates[i]/(100*52)))^((7/365)*52))
        SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
        
        
      }
  
    }
  
  # For SG/P 
  else if(trd==3)
  {	  # z is trading amount
      z=per*SVal
      
      us_sgp_trd=USInt
      uk_sgp_trd=UKVal + (z*sgp[i])
      uk_sgp_trd=uk_sgp_trd*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
      sg_sgp_trd=(SVal-z-(z*0.01))
      sg_sgp_trd=sg_sgp_trd*(1+((SRates[i]/(100*52)))^((7/365)*52))*sgd[i]
      
      
      if((USInt+UKInt+SInt)>=(us_sgp_trd+uk_sgp_trd+sg_sgp_trd))
      { #print(3)
        UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
        USVal=USInt
        SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
        
        
      } 
      else {  UKVal=(UKVal + (z*sgp[i]))*(1+((UKRates[i]/(100*52)))^((7/365)*52))
              USVal=USInt
              SVal=(SVal-z-(z*0.01))*(1+((SRates[i]/(100*52)))^((7/365)*52))
              
              
            }  
    }
  
  # D/SG
  else if(trd==4)
  {	
    # p is trading amount
    p=per*USVal
  
    us_dsg_trd=USVal-p-(p*0.01)
    us_dsg_trd=us_dsg_trd*(1+((USRates[i]/(100*52)))^((7/365)*52))
    sg_dsg_trd=SVal + (p*dsg[i])
    sg_dsg_trd=sg_dsg_trd*(1+((SRates[i]/(100*52)))^((7/365)*52))*sgd[i]
    uk_dsg_trd=UKInt
    
    if((USInt+UKInt+SInt)>=(us_dsg_trd+uk_dsg_trd+sg_dsg_trd))
      { #print(4)
        UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
        SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
        USVal=USInt
        
      } 
    else 
      {   
          SVal=(SVal + (p*dsg[i]))*(1+((SRates[i]/(100*52)))^((7/365)*52))
          USVal=(USVal-p-(p*0.01))*(1+((USRates[i]/(100*52)))^((7/365)*52))
          UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
          
          }
  }
  
  # D/P
  else if(trd==5)
  {	# q is trading amount
    q=per*USVal

    uk_dp_trd=UKVal + (q*dp[i])
    uk_dp_trd=uk_dp_trd*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
    us_dp_trd=USVal-q-(q*0.01)
    us_dp_trd=us_dp_trd*(1+((USRates[i]/(100*52)))^((7/365)*52))
    sg_dp_trd=SInt
    
    
    if((USInt+UKInt+SInt)>=(us_dp_trd+uk_dp_trd+sg_dp_trd))
    { #print(5)
      UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
      USVal=USInt
      SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
      
    } 
    else 
      { UKVal=(UKVal + (q*dp[i]))*(1+((UKRates[i]/(100*52)))^((7/365)*52))
        USVal=(USVal-q-(q*0.01))*(1+((USRates[i]/(100*52)))^((7/365)*52))
        SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
      
      }
  }
  
  else
  {	# r is trading amount
    r=per*UKVal

    us_psg_trd=USInt
    uk_psg_trd=UKVal-r-(r*0.01)
    uk_psg_trd=uk_psg_trd*(1+((UKRates[i]/(100*52)))^((7/365)*52))*pd[i]
    sg_psg_trd=SVal + (r*psg[i])
    sg_psg_trd=sg_psg_trd*(1+((SRates[i]/(100*52)))^((7/365)*52))*sgd[i]
    
    if((USInt+UKInt+SInt)>=(us_psg_trd+uk_psg_trd+sg_psg_trd))
    { #print(6)
      SVal=SVal*(1+((SRates[i]/(100*52)))^((7/365)*52))
      USVal=USInt
      UKVal=UKVal*(1+((UKRates[i]/(100*52)))^((7/365)*52))
    
    } 
    else 
      { 
        SVal=(SVal + (r*psg[i]))*(1+((SRates[i]/(100*52)))^((7/365)*52))
        UKVal=(UKVal-r-(r*0.01))*(1+((UKRates[i]/(100*52)))^((7/365)*52))
        USVal=USInt
      
      }
    }
  
  
  USProf = (USVal+(UKVal*pd[i])+(SVal*sgd[i]))-(USVal_Prev+(UKVal_Prev*pd[i-1])+(SVal_Prev*sgd[i-1]))
  
  # Total Profit calculated on a daily basis
  Total_Prof = Total_Prof + USProf
  
  print(Total_Prof)
  # print(UKProf)
  # print(SProf)
  
  
}

# Printing all the results
cat("Total US Dollars = ", USVal,"$")
cat("Total UK Pounds = ", UKVal,"£")
cat("Total Sing Dollars = ", SVal,"$")
cat("Total Profit in US Dollars = ", Total_Prof,"$")