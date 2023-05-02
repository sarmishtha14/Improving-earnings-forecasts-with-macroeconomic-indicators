library("ggplot2")
#install.packages("ggfortify") # install the package
library("ggfortify") 
#install.packages("parallel")
library("parallel")
# install the package
library("ggfortify") 
#install.packages("forecast")
library(forecast)
library("corrplot")
#install.packages("corrplot")
library("caret")
#install.packages("caret")
library("zoo")
#install.packages("zoo")
library("parallel")
library("corrplot")
library("lubridate")
library("data.table")
load("financialETFqtr.RDA")## load financial sector data merged with macro variables
colnames(financial) <- c("DATE","Sales_Revenue_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                         "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                         "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                         "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                         "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                         "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                         "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                         "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                         "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                         "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                         "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","realFEDFUNDS","Recession","PERMIT", "NMI","PMI", "annual_m2","unemploymentrate","US10")

##plot all macro against EPS to investigate relationship
ggplot(financial, aes(x = DATE, y = EPS_recurrent_earnings)) +
  geom_line(aes(y = PERMIT/100, color = "PERMIT")) +
  geom_line(aes(y = FEDFUNDS, color = "FEDFUNDS")) +
  geom_line(aes(y = unemploymentrate, color = "unemploymentrate")) +
  geom_line(aes(y = annual_m2/100, color = "annual_m2")) +
  geom_line(aes(y = PMI, color = "PMI")) +
  geom_line(aes(y = US10, color = "US10"))+
  geom_line(
    aes(y = EPS_recurrent_earnings), color = "orange",size = 1.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Macro variables against EPS_recurrent_earnings",
       y = "annual_m2 & PERMIT (in thousands)", color = "Legend Title") +
  theme_bw() +
  scale_color_manual(name = "Legend Title",
                     values = c("PERMIT" = "blue", "FEDFUNDS" = "red",
                                "unemploymentrate" = "green", "annual_m2" = "black",
                                "PMI" = "pink", "orange" = "orange","US10"="purple" ),
                     labels = c("PERMIT", "FEDFUNDS", "unemploymentrate",
                                "annual_m2", "PMI", "EPS_recurrent_earnings", "US10"))

##plot leading indicators  against EPS

ggplot(financial, aes(x = DATE, y = EPS_Change)) +
  geom_line(aes(y = unemploymentrate, color = "unemploymentrate")) +
  geom_line(aes(y = annual_m2/100, color = "annual_m2")) +
  geom_line(aes(y = US10, color = "US10"))+
  geom_line(
    aes(y = EPS_Change), color = "orange",size = 1.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months") +
  labs(title = "Macro variables against EPS_Change",
       color = "Legend Title") +
  theme_bw() +
  scale_color_manual(name = "Legend Title",
                     values = c("US10" = "blue", "unemploymentrate" = "black",
                                "annual_m2" = "red", "orange" = "orange"),
                     labels = c("annual_m2(in thousands)", "unemploymentrate", "US10","EPS_Change"))


###Correlation###


dataset_no_cor <- financial[,-high_cor]
colnames(dataset_no_cor)
corrplot(cor(as.matrix(financial[,-1])))

### EPS Seasonality
FIN_EPS<-ts(financial$EPS_recurrent_earnings, start=c(2001,4), frequency=4)
FIN_EPS1 <- window(FIN_EPS, start = c(2019,1),frequency = 4)
ggseasonplot(FIN_EPS1)
ggsubseriesplot(FIN_EPS1)


### EPS change Seasonality
FIN_EPSC<-ts(financial$EPS_Change, start=c(2001,4), frequency=4)
FIN_EPSC1 <- window(FIN_EPSC, start = c(2014,1), end=c(2017,4),frequency = 4)#
ggseasonplot(FIN_EPSC1)
ggsubseriesplot(FIN_EPSC1)

###modeling###
  
  # convert industry data to time series data
financial <- financial[month(financial$DATE) == 10,]
ac1<- as.data.table(financial) ##financial as datatable
ac2<-as.xts.data.table(ac1) ##financial as xts
Financial <- ts(ac1, start=c(2001 ) ,frequency=1) ##financial as ts
colnames(Financial) <- c("DATE","Sales_Revenue_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                         "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                         "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                         "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                         "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                         "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                         "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                         "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                         "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                         "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                         "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","realFEDFUNDS","Recession","PERMIT", "NMI","PMI", "annual_m2","unemploymentrate","us10")



# Split the data into training and testing sets
train_data <- window(Financial, start = c(2001), end = c(2018), frequency=1)
test_data <- window(Financial, start = c(2019),end = c(2022), frequency=1)

## after trying a combination of different predictors with high correlation with the dependent variable and low correlation between predictors we came up with two models
#model 4 and model 7 were the best model giving high rsquared and accuracy

#macro 0.8016429 
model4<- tslm(EPS_Change ~ Pretax_Income_Growth+Price_Earnings+us10 ,data = train_data)
summary(model4)
cvmodel4<-CV(model4)
cvmodel4

#NO macro 
model7<- tslm(EPS_Change ~ Pretax_Income_Growth+Price_Earnings,data = train_data)
summary(model7)
cvmodel7<-CV(model7)
cvmodel7





####Forecasting####
TD<-as.data.frame(test_data)##test data in dataframe
Forecastset<-forecast(model4, newdata=TD)
#plot(forecast(model5, newdata=TD, h=1))
Forecast<-forecast(model7, newdata=TD)



##Calculating EPS from forecasted EPS Change
EPS <- financial[18:21,"EPS_recurrent_earnings"]
EPS_shift <- financial[19:22,"EPS_recurrent_earnings"]

forcstEPSChg<- cbind(Forecastset$mean,Forecastset$lower , Forecastset$upper)
IND<-  cbind(Forecastset$mean,Forecastset$lower , Forecastset$upper, EPS, EPS_shift, TD$EPS_recurrent_earnings)
forcstEPSChg<- as.data.frame(forcstEPSChg)
IND<- as.data.frame(IND)
IND$EPS_fcast[1] <- (1+forcstEPSChg$`Forecastset$mean`[1])*IND$EPS_shift[1]
IND$EPS_fcast80h[1] <- (1+forcstEPSChg$`Forecastset$upper.1`[1])*IND$EPS_shift[1]#95% upper interval
IND$EPS_fcast80l[1] <- (1+forcstEPSChg$`Forecastset$lower.1`[1])*IND$EPS_shift[1]#95% lower interval
IND$EPS_fcast95h[1] <- (1+forcstEPSChg$`Forecastset$upper.2`[1])*IND$EPS_shift[1]#80% upper interval
IND$EPS_fcast95l[1] <- (1+forcstEPSChg$`Forecastset$lower.2`[1])*IND$EPS_shift[1]#80% lower interval

for(i in 2:4){
  IND$EPS_fcast[i] <- (1+forcstEPSChg$`Forecastset$mean`[i]) *IND$EPS_fcast[i-1]
  IND$EPS_fcast80h[i] <- (1+forcstEPSChg$`Forecastset$upper.1`[i]) *IND$EPS_fcast80h[i-1]
  IND$EPS_fcast80l[i] <- (1+forcstEPSChg$`Forecastset$lower.1`[i]) *IND$EPS_fcast80l[i-1]
  IND$EPS_fcast95h[i] <- (1+forcstEPSChg$`Forecastset$upper.2`[i])*IND$EPS_fcast95h[i-1]
  IND$EPS_fcast95l[i] <- (1+forcstEPSChg$`Forecastset$lower.2`[i])*IND$EPS_fcast95l[i-1]
  
}
forecast<-c( 2.599552, 2.286756, 4.910361, 4.306803)
forecastMacro<-c( 2.0466205, 0.9103023, 1.6739089, 1.6241905)

accuracy(IND$EPS_fcast, IND$`TD$EPS_recurrent_earnings`)


##analyst forecast accuracy
finestimates<-read.csv("C:/Users/user/Downloads/XLF estimates.csv")
accuracy(finestimates$EPS[11:14], IND$`TD$EPS_recurrent_earnings`)
#ME      RMSE       MAE       MPE     MAPE
#Test set -0.5094877 0.5893185 0.5094877 -28.66018 28.66018




###plotting forecast,forecastmacro,Actual_EPS,Analyst_forecast
final <- cbind(TD$EPS_recurrent_earnings,finestimates$EPS[11:14],forecast, forecastMacro )
final<-data.frame(final)
final<-cbind(financial$DATE[19:22], final)

colnames(final)<-c("Date","Actual_EPS", "Analyst_forecast","forecast", "forecastMacro")

ggplot(final, aes(x = Date, y = Actual_EPS)) +
  geom_line(aes(y = forecastMacro, color = "forecastMacro")) +
  geom_line(aes(y = Analyst_forecast, color ="Analyst_forecast" )) +
  geom_line(aes(y = forecast, color = "forecast"))+
  geom_line(
    aes(y = Actual_EPS), color = "red",size = 1.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Comparison of Forecasts against Actual EPS",
       color = "Legend Title") +
  theme_bw() +
  scale_color_manual(name = "Legend Title",
                     values = c("Actual_EPS" = "red", "Analyst_forecast" = "green", "forecast" = "purple",
                                "forecastMacro" = "blue"),
                     labels = c("Actual_EPS", "Analyst_forecast","forecast without Macro", "forecast with macro"))
