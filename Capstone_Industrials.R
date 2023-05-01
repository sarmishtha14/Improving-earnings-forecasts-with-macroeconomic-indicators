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
install.packages("corrplot")
library("caret")
#install.packages("caret")
library("zoo")
install.packages("zoo")
library("lubridate")
library("data.table")
#install.packages("corrplot")
library("corrplot")


load("industryETFqtr.RDA") #load industrial data with all macro indicators 
colnames(industry) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                        "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                        "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                        "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                        "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                        "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                        "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                        "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                        "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                        "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                        "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","realFEDFUNDS","Recession","PERMIT.x" ,"NMI","PMI", "annual_m2","unemploymentrate", "IPI")
industry$unemploymentrate<-as.numeric(industry$unemploymentrate)
industry$IPI<-as.numeric(industry$IPI)
industry$annual_m2<-as.numeric(industry$annual_m2)


##Corrletion##
cor_matrix <- corrplot(cor(as.matrix(industry[,-1])))

cor_matrix <- cor(as.matrix(industry[,-1]))
# Find pairs of variables with correlation above a certain threshold (e.g., 0.8)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.95)
# Remove the highly correlated variables from the dataset to prevent multicollinearity
dataset_no_cor <- industry[,-high_cor]
colnames(dataset_no_cor)


##plot all macro against EPS to insvestigate relationship
ggplot(industry, aes(x = DATE, y = EPS_recurrent_earnings)) +
  geom_line(aes(y = PERMIT.x/100, color = "PERMIT.x")) +
  geom_line(aes(y = FEDFUNDS, color = "FEDFUNDS")) +
  geom_line(aes(y = unemploymentrate, color = "unemploymentrate")) +
  geom_line(aes(y = annual_m2/100, color = "annual_m2")) +
  geom_line(aes(y = PMI, color = "PMI")) +
  geom_line(aes(y = IPI, color = "IPI"))+
  geom_line(
            aes(y = EPS_recurrent_earnings), color = "orange",size = 1.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months") +
  labs(title = "Macro variables against EPS_recurrent_earnings",
       y = "annual_m2 & PERMIT (in thousands)", color = "Legend Title") +
  theme_bw() +
  scale_color_manual(name = "Legend Title",
                     values = c("PERMIT.x" = "blue", "FEDFUNDS" = "red",
                                "unemploymentrate" = "green", "annual_m2" = "black",
                                "PMI" = "pink", "orange" = "orange"),
                     labels = c("PERMIT.x", "FEDFUNDS", "unemploymentrate",
                                "annual_m2", "PMI", "EPS_recurrent_earnings"))

##plot leading indicators  against EPS
 
ggplot(industry, aes(x = DATE, y = EPS_Change)) +
  geom_line(aes(y = PERMIT.x/100, color = "PERMIT.x")) +
  geom_line(aes(y = PMI, color = "PMI")) +
  geom_line(aes(y = IPI, color = "IPI"))+
  geom_line(
    aes(y = EPS_Change), color = "orange",size = 1.2) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "12 months") +
  labs(title = "Macro variables against EPS_Change",
        color = "Legend Title") +
  theme_bw() +
  scale_color_manual(name = "Legend Title",
                     values = c("PERMIT.x" = "blue", "IPI" = "black",
                                "PMI" = "red", "orange" = "orange"),
                     labels = c("PERMIT(in thousands)", "PMI", "IPI","EPS_Change"))


### EPS Seasonality
Ind_EPS<-ts(industry$EPS_recurrent_earnings, start=c(2001,4), frequency=4)
Ind_EPS1 <- window(Ind_EPS, start = c(2019,1),frequency = 4)
ggseasonplot(Ind_EPS1)
ggsubseriesplot(Ind_EPS1)


### EPS change Seasonality
Ind_EPSC<-ts(industry$EPS_Change, start=c(2001,4), frequency=4)
Ind_EPSC1 <- window(Ind_EPSC, start = c(2014,1), end=c(2017,4),frequency = 4)#
ggseasonplot(Ind_EPSC1)
ggsubseriesplot(Ind_EPSC1)




###modeling###
# convert industry data to time series data

industry <- industry[month(industry$DATE) == 10,] #turn data to annual data because the comparison done at the end of the analysis is aginst the annual forecast of the factset analysts
ac1<- as.data.table(industry) ##industry as datatable
ac2<-as.xts.data.table(ac1) ##industry as xts
Industry <- ts(ac1, start=c(2001 ) ,end=c(2022),frequency=1) ##industry as ts
colnames(Industry) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                        "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                        "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                        "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                        "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                        "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                        "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                        "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                        "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                        "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                        "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","realFEDFUNDS","Recession","PERMIT.x" ,"NMI","PMI", "annual_m2","unemploymentrate", "IPI")## as time series

# Split the data into training and testing sets
train_data <- window(Industry, start = c(2001), end = c(2018), frequency=1)
test_data <- window(Industry, start = c(2019), frequency=1)



## after trying a combination of different predictors with high correlation with the dependent variable and low correlation between predictors we came up with two models
#model 13 and model 3 were the best model giving high rsquared and accuracy

#model1
model1<- tslm(EPS_Change ~ COGS_Growth + Gross_Income_Growth + Pretax_Income_Growth + Dividends_Change + Gross_Margin + Sales_Revenue_Growth+EBIT_Growth
               + Price_Book_Value  + Price_Free_Cash_Flow + Enterprise_Value_EBITDA + Dividend_Yield +Net_Income_Growth+EBITDA_Growth+Dividend_Growth+annual_m2+unemploymentrate+IPI+NMI+PMI+Recession
              +EBIT_Margin+Price_Earnings+Price_Cash_Flow+Price_Cash_Flow_excl+Enterprise_Value_Sales+Enterprise_Value_EBIT+Pretax_Margin
                +Price_Earnings_excl+Sales_Per_Share+Book_Value_Per_Share+Cash_Flow_Per_Share+realFEDFUNDS+EPS_recurrent_earnings+Price_Sales+Return_on_Equity+Free_Cash_Flow_Per_Share+Free_Cash_Flow_Margin
                 +Dividends_Per_Share + FEDFUNDS + inflation_rate + PERMIT.x+ Net_Margin, data = train_data)
summary(model1)
cvmodel1<-CV(model1)


#model 13 with Macro
model13<- tslm(EPS_Change ~Gross_Income_Growth+COGS_Growth+Sales_Revenue_Growth+Net_Margin+Recession+Net_Margin+ Pretax_Margin+Pretax_Income_Growth+EBITDA_Growth+EBIT_Growth  +IPI+PMI,data = train_data)
summary(model13)
cvmodel13<-CV(model13)
cvmodel13
#model4


##variable corr with eps change
#model3 MAE- (no macro)
model3<- tslm(EPS_Change ~ Gross_Income_Growth+ COGS_Growth+Sales_Revenue_Growth+Net_Margin+Net_Margin+ Pretax_Margin+Pretax_Income_Growth+EBITDA_Growth + EBIT_Growth,data = train_data)
summary(model3)
cvmodel3<-CV(model3)
cvmodel3




#model10 MAE- 0.1499328
model10<- tslm(EPS_Change ~ Pretax_Income_Growth+ Net_Income_Growth+EBITDA_Growth+
                 Recession+NMI+PERMIT.x + IPI + PMI,data = train_data)
summary(model10)
cvmodel10<-CV(model10)
cvmodel10

## accuracy did not improve after using random forest model
# #install.packages("randomForest")
# library("randomForest")
# #model5
# model5 <- randomForest(EPS_Change ~ Gross_Income_Growth+COGS_Growth+Sales_Revenue_Growth+Net_Margin+Recession+Net_Margin+ Pretax_Margin+Pretax_Income_Growth+EBITDA_Growth+EBIT_Growth  +IPI+PMI, data=train_data, ntree=500, mtry=3)
# summary(model5)
# predictions <- predict(model5, newdata=test_data)





####Forecasting####
TD<-as.data.frame(test_data)##test data in dataframe
Forecastset<-forecast(model13, newdata=TD)


##Calculating EPS from forecasted EPS Change
EPS <- industry[19:22,"EPS_recurrent_earnings"]
EPS_shift <- industry[18:21,"EPS_recurrent_earnings"]
#  5columns of forecasted Eps chg 
forcstEPSChg<- cbind(Forecastset$mean,Forecastset$lower , Forecastset$upper)
IND<-cbind(Forecastset$mean,Forecastset$lower , Forecastset$upper, EPS, EPS_shift)
forcstEPSChg<- as.data.frame(forcstEPSChg)
IND<- as.data.frame(IND)
IND$EPS_fcast[1] <- (1+forcstEPSChg$`Forecastset$mean`[1])*IND$EPS_shift[1]
IND$EPS_fcast80h[1] <- (1+forcstEPSChg$`Forecastset$upper.1`[1])*IND$EPS_shift[1]
IND$EPS_fcast80l[1] <- (1+forcstEPSChg$`Forecastset$lower.1`[1])*IND$EPS_shift[1]
IND$EPS_fcast95h[1] <- (1+forcstEPSChg$`Forecastset$upper.2`[1])*IND$EPS_shift[1]
IND$EPS_fcast95l[1] <- (1+forcstEPSChg$`Forecastset$lower.2`[1])*IND$EPS_shift[1]

for(i in 2:4){
  IND$EPS_fcast[i] <- (1+forcstEPSChg$`Forecastset$mean`[i]) *IND$EPS_fcast[i-1]
  IND$EPS_fcast80h[i] <- (1+forcstEPSChg$`Forecastset$upper.1`[i]) *IND$EPS_fcast80h[i-1]
  IND$EPS_fcast80l[i] <- (1+forcstEPSChg$`Forecastset$lower.1`[i]) *IND$EPS_fcast80l[i-1]
  IND$EPS_fcast95h[i] <- (1+forcstEPSChg$`Forecastset$upper.2`[i])*IND$EPS_fcast95h[i-1]
  IND$EPS_fcast95l[i] <- (1+forcstEPSChg$`Forecastset$lower.2`[i])*IND$EPS_fcast95l[i-1]

}
accuracy(IND$EPS_fcast, TD$EPS_recurrent_earnings)
   #ME     RMSE     MAE       MPE     MAPE





###analyst accuracy
Indestimates<-read.csv("C:/Users/user/Downloads/XLI estimates.csv")
accuracy(Indestimates$EPS[11:14], TD$EPS_recurrent_earnings)
#ME      RMSE       MAE       MPE   MAPE
#0.072822 0.3254263 0.2971415 0.2645963 9.4942




##plotting forecast,forecastmacro,Actual_EPS,Analyst_forecast


forecast<-c(4.562372, 3.299964, 5.966098, 7.205272)#EPS from model with no macro var
forecastmacro<-c(3.931323, 1.947925 ,3.745679, 3.617153)#EPS from model with  macro var
final <- cbind(TD$EPS_recurrent_earnings,Indestimates$EPS[11:14],forecast, forecastmacro )
final<-data.frame(final)
final<-cbind(industry$DATE[19:22], final)

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





