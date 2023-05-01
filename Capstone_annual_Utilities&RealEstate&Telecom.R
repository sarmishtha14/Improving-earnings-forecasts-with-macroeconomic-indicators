library('MASS')
library('stats')
library("readxl")
library("dplyr")
library("data.table")
library(gplots)
library("ggplot2")
library("ggfortify") 
library(forecast)
library(caret)
library(foreign)
library(xts)
library(corrplot)

#Loading the forecast factset data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/factset_forecasts.rda")
combined_forecasts <- combined_forecasts[,-1]

#Real Estate
RealEstate_analyst <- combined_forecasts[combined_forecasts$Sector == "Real Estate",]
RealEstate_3 <- RealEstate_analyst[12:14,]

#Utilities
CS_analyst <- combined_forecasts[combined_forecasts$Sector == "Utilities",]
CS_3 <- CS_analyst[12:14,]

#TeleCommunication
C_analyst <- combined_forecasts[combined_forecasts$Sector == "Telecommunication",]
C_3 <- C_analyst[12:14,]

#Loading the factset data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/factset_actuals.rda")

#Converting percentage terms to actual terms
combined_actuals[,1:16] <- combined_actuals[,1:16]/100

#Loading the Housing Index data and merge it
Housing_index <- read.csv("C:/Users/Sarmishtha Jain/Downloads/monthlyPERMIT.csv")
Housing_index$DATE <- as.Date(Housing_index$DATE)
combined_actuals$DATE <- as.Date(combined_actuals$DATE)
combined_actuals <- merge(combined_actuals, Housing_index, by.x = "DATE",by.y = "DATE")
PMI <- read.csv("C:/Users/Sarmishtha Jain/Downloads/PMI_NMI DATA.csv", header = TRUE) #Purchasing Managers' Index (PMI)
PMI$Date <- as.Date(PMI$Date)
combined_actuals <- merge(combined_actuals, PMI, by.x = "DATE",by.y = "Date")
combined_actuals$`inflation rate` <- na.spline(combined_actuals$`inflation rate`, along = combined_actuals$DATE, na.rm = FALSE)

#Exploratory Data Analysis

#Seasonal trend of EPS earnings
plotmeans(combined_actuals$`EPS (recurrent earnings)` ~ year(combined_actuals$DATE), main="Heterogeineityacross years", data=combined_actuals)

#Trend of EPS Growth
plotmeans(combined_actuals$`EPS Change` ~ year(combined_actuals$DATE), main="Heterogeineityacross years", data=combined_actuals)

#Sector wise seasonality
plotmeans(combined_actuals[combined_actuals$Industrials ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$Industrials ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$Industrials == 1,],xlab = "EPS Growth",ylab = "Year (Industrials)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$Financials ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$Financials ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$Financials == 1,],xlab = "EPS Growth",ylab = "Year (Financials)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$Utilities ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$Utilities ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$Utilities == 1,],ylab = "EPS Growth",xlab = "Year (Utilities)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$Energy ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$Energy ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$Energy == 1,],ylab = "EPS Growth",xlab = "Year (Energy)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$InformationTechnology ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$InformationTechnology ==1,]$DATE), main="Heterogeineity across years", data=combined_actuals[combined_actuals$InformationTechnology == 1,],ylab = "EPS Growth",xlab = "Year (Technology)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$Materials ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$Materials ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$Materials == 1,],ylab = "EPS Growth",xlab = "Year (Materials)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$RealEstate ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$RealEstate ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$RealEstate == 1,],ylab = "EPS Growth",xlab = "Year (Real Estate)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$CommunicationServices ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$CommunicationServices ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$CommunicationServices == 1,],ylab = "EPS Growth",xlab = "Year (Communication Services)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$ConsumerDiscretionary ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$ConsumerDiscretionary ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$ConsumerDiscretionary == 1,],ylab = "EPS Growth",xlab = "Year (Consumer Discretionary)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$ConsumerStaples ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$ConsumerStaples ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$ConsumerStaples == 1,],ylab = "EPS Growth",xlab = "Year (Consumer Staples)",ylim = c(-500,500))
plotmeans(combined_actuals[combined_actuals$HealthCare ==1,]$`EPS Change` ~ year(combined_actuals[combined_actuals$HealthCare ==1,]$DATE), main="Heterogeineityacross years", data=combined_actuals[combined_actuals$HealthCare == 1,],ylab = "EPS Growth",xlab = "Year (HealthCare)",ylim = c(-500,500))

#Adding dummy variable for EPS recession
combined_actuals$Recession <- ifelse(year(combined_actuals$DATE) == 2001 | year(combined_actuals$DATE) == 2008 | year(combined_actuals$DATE) == 2009 | year(combined_actuals$DATE) == 2020 | year(combined_actuals$DATE) == 2017,1,0)

#EPSG and NIG are highly correlated, so we can remove NIG
#DC and DG are highly correlated, so we can remove DC

#Timeseries/Categorical/Continuous/Discrete

#Real Estate Sector EDA

#RealEstate Industry
RealEstate <- combined_actuals[combined_actuals$Sector=="Real Estate",]
RealEstate<-RealEstate[,-(39:50)]
RealEstate$DATE <- as.Date(RealEstate$DATE)
RealEstate <- RealEstate[month(RealEstate$DATE) == 10,]

#Exploratory data analysis
ggplot(data = RealEstate, mapping = aes(x=RealEstate$DATE )) +
  geom_line(mapping = aes(y = RealEstate$`COGS Growth`), color = "blue") +
  geom_line(aes(y = RealEstate$`inflation rate`), color = "red") +
  geom_line(aes(y = RealEstate$`EBIT Growth`), color = "green") +
  geom_line(aes(y = RealEstate$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = RealEstate$`EPS Change`), color = "grey") 

#EPS Spikes in the years 2003-2004, 2011-2012, 2018 and 2022
#EPS Steeps in the years 2008-2009 and 2020 (recession)
#EBIT Growth shows similar trend to EPS

#Correlations

RealEstate <- data.table(RealEstate)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetM <- RealEstate[, c("Sales/Revenue Growth","COGS Growth","Gross Income Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
                                 "Net Income Growth", "Dividend Growth", "EPS Change", "Dividends Change", 
                                 "Gross Margin", "EBIT Margin", "EBITDA Margin", "Pretax Margin", 
                                 "Net Margin", "Free Cash Flow Margin", "Price/Earnings", 
                                 "Price/Earnings (x) (excl negatives)", "Price/Sales (x)", "Price/Book Value (x)", 
                                 "Price/Cash Flow (x)", "Price/Cash Flow (x) (excl negatives)", 
                                 "Price/Free Cash Flow (x)", "Enterprise Value/EBIT (x)", 
                                 "Enterprise Value/EBITDA (x)", "Enterprise Value/Sales (x)", 
                                 "Return on Equity (%)", "Dividend Yield (%)", "EPS (recurrent earnings)",
                                 "Sales Per Share", "Dividends Per Share", "Book Value Per Share", 
                                 "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT","Recession")]
colnames(RealEstate) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                         "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                         "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                         "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                         "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                         "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                         "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                         "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                         "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                         "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                         "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","PERMIT","Non.Manufacturing.Index..NMI.","Purchasing.Managers..Index..PMI.","Recession")
colnames(my_data_subsetM) <- c("Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                               "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                               "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                               "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                               "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                               "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                               "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                               "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                               "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                               "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT","Recession")

EPS_RealEstate <- RealEstate[20:22,"EPS_recurrent_earnings"]
EPS_RealEstate_shift <- RealEstate[19:21,"EPS_recurrent_earnings"]


# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingM <- cor(as.matrix(my_data_subsetM))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsM <- findCorrelation(cor_matrix_remainingM, cutoff = 0.7)
highly_correlated_varsM <- highly_correlated_varsM[highly_correlated_varsM!=9]

# Remove the highly correlated variables from the data table
my_data_subsetM <- my_data_subsetM[, -highly_correlated_varsM, with = FALSE]
varM <- colnames(my_data_subsetM)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetM)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetM,RealEstate[,c(1,10,37)], by = "EPS_Change")
m1 <- m1[order(DATE)]
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
RealEstate_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
RealEstate_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#Seasonality
autoplot(RealEstate_Eps_ts1)

#Modeling for RealEstate Sector
#Linear model
model_RealEstate <- lm(EPS_Change~ ., data = my_data_subsetM)
summary(model_RealEstate)
CV(model_RealEstate)

#Timeseries model with Macro
model_RealEstate_ts <- tslm(EPS_Change ~ EBIT_Growth+              
                             Dividends_Change+EBIT_Margin+Pretax_Margin+           
                             Price_Free_Cash_Flow+Enterprise_Value_Sales+        
                             Dividends_Per_Share+Book_Value_Per_Share+Cash_Flow_Per_Share+Free_Cash_Flow_Per_Share+
                             PERMIT+Recession+inflation_rate+FEDFUNDS,data = RealEstate_Eps_ts1)

#In sample accuracy
summary(model_RealEstate_ts)
CV(model_RealEstate_ts)
x_M <- RealEstate_Eps_ts2
x_M <- as.data.frame(x_M)

#Finding the Forecasted EPS from EPS Growth
forecast_M <- forecast(model_RealEstate_ts,newdata = x_M, h = 1)
plot(forecast_M)
RealEstate_Eps_ts2 <- cbind(RealEstate_Eps_ts2,forecast_M,EPS_RealEstate,EPS_RealEstate_shift)
RealEstate_Eps_df <- as.data.frame(RealEstate_Eps_ts2)

RealEstate_Eps_df$EPS_fcast[1] <- (1+RealEstate_Eps_df$`forecast_M.Point Forecast`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast80h[1] <- (1+RealEstate_Eps_df$`forecast_M.Hi 80`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast80l[1] <- (1+RealEstate_Eps_df$`forecast_M.Lo 80`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast95h[1] <- (1+RealEstate_Eps_df$`forecast_M.Hi 95`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast95l[1] <- (1+RealEstate_Eps_df$`forecast_M.Lo 95`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]

for(i in 2:3){
  RealEstate_Eps_df$EPS_fcast[i] <- (1+RealEstate_Eps_df$`forecast_M.Point Forecast`[i]) *RealEstate_Eps_df$EPS_fcast[i-1]
  RealEstate_Eps_df$EPS_fcast80h[i] <- (1+RealEstate_Eps_df$`forecast_M.Hi 80`[i]) *RealEstate_Eps_df$EPS_fcast80h[i-1]
  RealEstate_Eps_df$EPS_fcast80l[i] <- (1+RealEstate_Eps_df$`forecast_M.Lo 80`[i]) *RealEstate_Eps_df$EPS_fcast80l[i-1]
  RealEstate_Eps_df$EPS_fcast95h[i] <- (1+RealEstate_Eps_df$`forecast_M.Hi 95`[i])*RealEstate_Eps_df$EPS_fcast95h[i-1]
  RealEstate_Eps_df$EPS_fcast95l[i] <- (1+RealEstate_Eps_df$`forecast_M.Lo 95`[i])*RealEstate_Eps_df$EPS_fcast95l[i-1]
}

#Out of Sample accuracy of Real Estate with Macro
RealEstate_Eps_df <- cbind(RealEstate_Eps_df,RealEstate$DATE[20:22])
Mat_Macro <-accuracy(RealEstate_Eps_df$EPS_fcast,RealEstate_Eps_df$EPS_RealEstate)

#Accuracy of Real Estate forecasts
RealEstate_Eps_df$`RealEstate$DATE[20:22]` <- as.character(RealEstate_Eps_df$`RealEstate$DATE[20:22]` )
RealEstate_Eps_df <- merge(RealEstate_Eps_df,RealEstate_analyst, by.x = "RealEstate$DATE[20:22]", by.y = "DATE")
Mat_Macro_fcast <-accuracy(as.numeric(RealEstate_Eps_df$EPS),RealEstate_Eps_df$EPS_RealEstate)

#adding to a db
RE_fcast <- data.frame(c("Date","EPS","Data_Type"))
RE_fcast$Date <-  RealEstate_Eps_df$`RealEstate$DATE[20:22]`
RE_fcast$EPS <- RealEstate_Eps_df$EPS_fcast
RE_fcast$Data_Type <- "Forecast with Macro"

RE_analyst <- data.frame(c("Date","EPS","Data_Type"))
RE_analyst$Date <-  RealEstate_Eps_df$`RealEstate$DATE[20:22]`
RE_analyst$EPS <- RealEstate_Eps_df$EPS
RE_analyst$Data_Type <- "Analyst Forecast"

RE_actual <- data.frame(c("Date","EPS","Data_Type"))
RE_actual$Date <-  RealEstate_Eps_df$`RealEstate$DATE[20:22]`
RE_actual$EPS <- RealEstate_Eps_df$EPS_RealEstate
RE_actual$Data_Type <- "Actual EPS"


#Timeseries model without macro
RealEstate_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
model_RealEstate_ts <- tslm(EPS_Change ~ EBIT_Growth+              
                             Dividends_Change+EBIT_Margin+Pretax_Margin+           
                             Price_Free_Cash_Flow+Enterprise_Value_Sales+        
                             Dividends_Per_Share+Book_Value_Per_Share+Cash_Flow_Per_Share+Free_Cash_Flow_Per_Share+
                             Recession,data = RealEstate_Eps_ts1)

#In sample accuracy of model without macro
summary(model_RealEstate_ts)
CV(model_RealEstate_ts)
RealEstate_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
x_M <- RealEstate_Eps_ts2
x_M <- as.data.frame(x_M)

#Finding the Forecasted EPS from EPS Growth
forecast_M <- forecast(model_RealEstate_ts,newdata = x_M, h = 1)
plot(forecast_M)
RealEstate_Eps_ts2 <- cbind(RealEstate_Eps_ts2,forecast_M,EPS_RealEstate,EPS_RealEstate_shift)
RealEstate_Eps_df <- as.data.frame(RealEstate_Eps_ts2)

RealEstate_Eps_df$EPS_fcast[1] <- (1+RealEstate_Eps_df$`forecast_M.Point Forecast`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast80h[1] <- (1+RealEstate_Eps_df$`forecast_M.Hi 80`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast80l[1] <- (1+RealEstate_Eps_df$`forecast_M.Lo 80`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast95h[1] <- (1+RealEstate_Eps_df$`forecast_M.Hi 95`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]
RealEstate_Eps_df$EPS_fcast95l[1] <- (1+RealEstate_Eps_df$`forecast_M.Lo 95`[1])*RealEstate_Eps_df$EPS_RealEstate_shift[1]

for(i in 2:3){
  RealEstate_Eps_df$EPS_fcast[i] <- (1+RealEstate_Eps_df$`forecast_M.Point Forecast`[i]) *RealEstate_Eps_df$EPS_fcast[i-1]
  RealEstate_Eps_df$EPS_fcast80h[i] <- (1+RealEstate_Eps_df$`forecast_M.Hi 80`[i]) *RealEstate_Eps_df$EPS_fcast80h[i-1]
  RealEstate_Eps_df$EPS_fcast80l[i] <- (1+RealEstate_Eps_df$`forecast_M.Lo 80`[i]) *RealEstate_Eps_df$EPS_fcast80l[i-1]
  RealEstate_Eps_df$EPS_fcast95h[i] <- (1+RealEstate_Eps_df$`forecast_M.Hi 95`[i])*RealEstate_Eps_df$EPS_fcast95h[i-1]
  RealEstate_Eps_df$EPS_fcast95l[i] <- (1+RealEstate_Eps_df$`forecast_M.Lo 95`[i])*RealEstate_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Real Estate without Macro
RealEstate_Eps_df <- cbind(RealEstate_Eps_df,RealEstate$DATE[20:22])
Mat_Macro_N <-accuracy(RealEstate_Eps_df$EPS_fcast,RealEstate_Eps_df$EPS_RealEstate)

RE_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
RE_fcast_M$Date <-  as.character(RealEstate_Eps_df$`RealEstate$DATE[20:22]`)
RE_fcast_M$EPS <- RealEstate_Eps_df$EPS_fcast
RE_fcast_M$Data_Type <- "Forecast Without Macro"

##Plot
RE <- rbind(RE_fcast,RE_fcast_M,RE_analyst,RE_actual)
RE$Date <- as.Date(RE$Date)
RE$EPS <- as.numeric(RE$EPS)
RE <- RE[,-1]

ggplot(RE, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


##################################################################################################################
#Utilities EDA

#Utilities ts
Utilities <- combined_actuals[combined_actuals$Sector=="Utilities",]
Utilities<-Utilities[,-(39:50)]
Utilities$DATE <- as.Date(Utilities$DATE)
Utilities <- Utilities[month(Utilities$DATE) == 10,]

#Exploratory data analysis
ggplot(data = Utilities, mapping = aes(x=Utilities$DATE )) +
  geom_line(mapping = aes(y = Utilities$`COGS Growth`), color = "blue") +
  geom_line(aes(y = Utilities$`inflation rate`), color = "red") +
  geom_line(aes(y = Utilities$`EBIT Growth`), color = "green") +
  geom_line(aes(y = Utilities$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = Utilities$`EPS Change`), color = "grey") 


#Correlations

Utilities <- data.table(Utilities)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetCS <- Utilities[, c("Sales/Revenue Growth","COGS Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
                                        "Net Income Growth", "Dividend Growth", "EPS Change", "Dividends Change", 
                                        "Gross Margin", "EBIT Margin", "EBITDA Margin", "Pretax Margin", 
                                        "Net Margin", "Free Cash Flow Margin", "Price/Earnings", 
                                        "Price/Earnings (x) (excl negatives)", "Price/Sales (x)", "Price/Book Value (x)", 
                                        "Price/Cash Flow (x)", "Price/Cash Flow (x) (excl negatives)", 
                                        "Price/Free Cash Flow (x)", "Enterprise Value/EBIT (x)", 
                                        "Enterprise Value/EBITDA (x)", "Enterprise Value/Sales (x)", 
                                        "Return on Equity (%)", "Dividend Yield (%)", "EPS (recurrent earnings)",
                                        "Sales Per Share", "Dividends Per Share", "Book Value Per Share", 
                                        "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT","Recession")]

colnames(Utilities) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                               "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                               "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                               "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                               "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                               "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                               "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                               "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                               "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                               "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","PERMIT","Non.Manufacturing.Index..NMI.","Purchasing.Managers..Index..PMI.","Recession")
colnames(my_data_subsetCS) <- c("Sales_Revenue_Growth","COGS_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                                "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                                "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                                "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                                "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                                "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                                "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                                "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                                "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                                "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                                "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT","Recession")

EPS_CS <- Utilities[20:22,"EPS_recurrent_earnings"]
EPS_CS_shift <- Utilities[19:21,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingCS <- cor(as.matrix(my_data_subsetCS))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsCS <- findCorrelation(cor_matrix_remainingCS, cutoff = 0.8)
highly_correlated_varsCS <- highly_correlated_varsCS[highly_correlated_varsCS!=9]

# Remove the highly correlated variables from the data table
my_data_subsetCS <- my_data_subsetCS[, -highly_correlated_varsCS, with = FALSE]
varCS <- colnames(my_data_subsetCS)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetCS)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetCS,Utilities[,c(1,10)], by = "EPS_Change")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
Utilities_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
Utilities_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#Modeling for Utilities Sector
#Linear model
model_conStaples <- lm(EPS_Change  ~ ., data = my_data_subsetCS)
summary(model_conStaples)
CV(model_conStaples)

#Timeseries model with macros
model_CS_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+EBIT_Growth+Dividends_Change     
                    +Gross_Margin+EBIT_Margin+Pretax_Margin+ Price_Earnings_excl      
                    +Price_Book_Value + Price_Free_Cash_Flow+Return_on_Equity+Cash_Flow_Per_Share
                    +FEDFUNDS+PERMIT +Recession,data = Utilities_Eps_ts1)

#In sample accuracy of model with macros
summary(model_CS_ts)
CV(model_CS_ts)
x_CS <- Utilities_Eps_ts2[1:3,varCS]
x_CS <- as.data.frame(x_CS)

#Finding the Forecasted EPS from EPS Growth
forecast_CS <- forecast(model_CS_ts,newdata = x_CS, h = 1)
plot(forecast_CS)
Utilities_Eps_ts2 <- cbind(Utilities_Eps_ts2,forecast_CS,EPS_CS,EPS_CS_shift)
CS_Eps_df <- as.data.frame(Utilities_Eps_ts2)

CS_Eps_df$EPS_fcast[1] <- (1+CS_Eps_df$`forecast_CS.Point Forecast`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast80h[1] <- (1+CS_Eps_df$`forecast_CS.Hi 80`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast80l[1] <- (1+CS_Eps_df$`forecast_CS.Lo 80`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast95h[1] <- (1+CS_Eps_df$`forecast_CS.Hi 95`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast95l[1] <- (1+CS_Eps_df$`forecast_CS.Lo 95`[1])*CS_Eps_df$EPS_CS_shift[1]

for(i in 2:3){
  CS_Eps_df$EPS_fcast[i] <- (1+CS_Eps_df$`forecast_CS.Point Forecast`[i]) *CS_Eps_df$EPS_fcast[i-1]
  CS_Eps_df$EPS_fcast80h[i] <- (1+CS_Eps_df$`forecast_CS.Hi 80`[i]) *CS_Eps_df$EPS_fcast80h[i-1]
  CS_Eps_df$EPS_fcast80l[i] <- (1+CS_Eps_df$`forecast_CS.Lo 80`[i]) *CS_Eps_df$EPS_fcast80l[i-1]
  CS_Eps_df$EPS_fcast95h[i] <- (1+CS_Eps_df$`forecast_CS.Hi 95`[i])*CS_Eps_df$EPS_fcast95h[i-1]
  CS_Eps_df$EPS_fcast95l[i] <- (1+CS_Eps_df$`forecast_CS.Lo 95`[i])*CS_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Utilities with Macro
CS_Eps_df <- cbind(CS_Eps_df,Utilities$DATE[20:22])
ConStap_Macro <- accuracy(CS_Eps_df$EPS_fcast,CS_Eps_df$EPS_CS)

#Accuracy of Utilities forecast
CS_Eps_df$`Utilities$DATE[20:22]` <- as.character(CS_Eps_df$`Utilities$DATE[20:22]` )
CS_Eps_df <- merge(CS_Eps_df,CS_analyst, by.x = "Utilities$DATE[20:22]", by.y = "DATE")
ConStap_fcast_Macro_ <- accuracy(as.numeric(CS_Eps_df$EPS),CS_Eps_df$EPS_CS)

#adding to a db
CS_fcast <- data.frame(c("Date","EPS","Data_Type"))
CS_fcast$Date <-  CS_Eps_df$`Utilities$DATE[20:22]`
CS_fcast$EPS <- CS_Eps_df$EPS_fcast
CS_fcast$Data_Type <- "Forecast With Macro"

CS_anal <- data.frame(c("Date","EPS","Data_Type"))
CS_anal$Date <-  CS_Eps_df$`Utilities$DATE[20:22]`
CS_anal$EPS <- CS_Eps_df$EPS
CS_anal$Data_Type <- "Analyst Forecast"

CS_actual <- data.frame(c("Date","EPS","Data_Type"))
CS_actual$Date <-  CS_Eps_df$`Utilities$DATE[20:22]`
CS_actual$EPS <- CS_Eps_df$EPS_CS
CS_actual$Data_Type <- "Actual EPS"


####################Timeseries model without macros
model_CS_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+EBIT_Growth+Dividends_Change     
                    +Gross_Margin+EBIT_Margin+Pretax_Margin+Free_Cash_Flow_Margin+Price_Earnings+Price_Earnings_excl       
                      +Price_Book_Value + Price_Free_Cash_Flow+Return_on_Equity+Sales_Per_Share+Cash_Flow_Per_Share
                    +Recession,data = Utilities_Eps_ts1)

#In sample accuract of model without macros
summary(model_CS_ts)
CV(model_CS_ts)
varCS <- varCS[-c(16:18)]
Utilities_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
x_CS <- Utilities_Eps_ts2[,varCS]
x_CS <- as.data.frame(x_CS)

#Finding the Forecasted EPS from EPS Growth
forecast_CS <- forecast(model_CS_ts,newdata = x_CS, h = 1)
plot(forecast_CS)
Utilities_Eps_ts2 <- cbind(Utilities_Eps_ts2,forecast_CS,EPS_CS,EPS_CS_shift)
CS_Eps_df <- as.data.frame(Utilities_Eps_ts2)

CS_Eps_df$EPS_fcast[1] <- (1+CS_Eps_df$`forecast_CS.Point Forecast`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast80h[1] <- (1+CS_Eps_df$`forecast_CS.Hi 80`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast80l[1] <- (1+CS_Eps_df$`forecast_CS.Lo 80`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast95h[1] <- (1+CS_Eps_df$`forecast_CS.Hi 95`[1])*CS_Eps_df$EPS_CS_shift[1]
CS_Eps_df$EPS_fcast95l[1] <- (1+CS_Eps_df$`forecast_CS.Lo 95`[1])*CS_Eps_df$EPS_CS_shift[1]

for(i in 2:3){
  CS_Eps_df$EPS_fcast[i] <- (1+CS_Eps_df$`forecast_CS.Point Forecast`[i]) *CS_Eps_df$EPS_fcast[i-1]
  CS_Eps_df$EPS_fcast80h[i] <- (1+CS_Eps_df$`forecast_CS.Hi 80`[i]) *CS_Eps_df$EPS_fcast80h[i-1]
  CS_Eps_df$EPS_fcast80l[i] <- (1+CS_Eps_df$`forecast_CS.Lo 80`[i]) *CS_Eps_df$EPS_fcast80l[i-1]
  CS_Eps_df$EPS_fcast95h[i] <- (1+CS_Eps_df$`forecast_CS.Hi 95`[i])*CS_Eps_df$EPS_fcast95h[i-1]
  CS_Eps_df$EPS_fcast95l[i] <- (1+CS_Eps_df$`forecast_CS.Lo 95`[i])*CS_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Utilities without Macro
CS_Eps_df <- cbind(CS_Eps_df,Utilities$DATE[20:22])
ConStap_Macro_N <- accuracy(as.numeric(CS_Eps_df$EPS_fcast),CS_Eps_df$EPS_CS)

#DF
CS_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
CS_fcast_M$Date <- as.character(CS_Eps_df$`Utilities$DATE[20:22]`)
CS_fcast_M$EPS <- CS_Eps_df$EPS_fcast
CS_fcast_M$Data_Type <- "Forecast Without Macro"

##plot
CS <- rbind(CS_fcast,CS_fcast_M,CS_anal,CS_actual)
CS$Date <- as.Date(CS$Date)
CS$EPS <- as.numeric(CS$EPS)
CS <- CS[,-1]

ggplot(CS, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

################################################################################################

#Communication EDA

#Communication ts
Communication <- combined_actuals[combined_actuals$Sector=="Telecommunication",]
Communication$Year <- year(Communication$DATE)
getSymbols("M2SL", src = "FRED") # M2 Money Stock
yearmon <- year(time(M2SL))
annual_m2 <- aggregate(M2SL, as.numeric(yearmon),mean)
Year <- rownames(as.data.frame(annual_m2))
annual_m2 <- cbind(annual_m2,Year)
Communication <- merge(Communication,annual_m2,by = "Year")
Communication<-Communication[,-(40:51)]
Communication$DATE <- as.Date(Communication$DATE)
Communication <- Communication[month(Communication$DATE) == 10,]
Communication$annual_m2 <- as.numeric(Communication$annual_m2)

#Exploratory data analysis
ggplot(data = Communication, mapping = aes(x=Communication$DATE )) +
  geom_line(mapping = aes(y = Communication$`COGS Growth`), color = "blue") +
  geom_line(aes(y = Communication$`inflation rate`), color = "red") +
  geom_line(aes(y = Communication$`EBIT Growth`), color = "green") +
  geom_line(aes(y = Communication$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = Communication$`EPS Change`), color = "grey") 

#Correlations

Communication <- data.table(Communication)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetC <- Communication[, c("Sales/Revenue Growth","COGS Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
                                     "Net Income Growth", "Dividend Growth", "EPS Change", "Dividends Change", 
                                     "Gross Margin", "EBIT Margin", "EBITDA Margin", "Pretax Margin", 
                                     "Net Margin", "Free Cash Flow Margin", "Price/Earnings", 
                                     "Price/Earnings (x) (excl negatives)", "Price/Sales (x)", "Price/Book Value (x)", 
                                     "Price/Cash Flow (x)", "Price/Cash Flow (x) (excl negatives)", 
                                     "Price/Free Cash Flow (x)", "Enterprise Value/EBIT (x)", 
                                     "Enterprise Value/EBITDA (x)", "Enterprise Value/Sales (x)", 
                                     "Return on Equity (%)", "Dividend Yield (%)", "EPS (recurrent earnings)",
                                     "Sales Per Share", "Dividends Per Share", "Book Value Per Share", 
                                     "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT","Recession","annual_m2")]

colnames(Communication) <- c("Year","DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                             "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                             "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                             "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                             "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                             "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                             "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                             "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                             "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                             "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                             "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","PERMIT","Non.Manufacturing.Index..NMI.","Purchasing.Managers..Index..PMI.","Recession","annual_m2")
colnames(my_data_subsetC) <- c("Sales_Revenue_Growth","COGS_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                               "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                               "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                               "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                               "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                               "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                               "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                               "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                               "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                               "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT","Recession","annual_m2")

EPS_C <- Communication[20:22,"EPS_recurrent_earnings"]
EPS_C_shift <- Communication[19:21,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingC <- cor(as.matrix(my_data_subsetC))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsC <- findCorrelation(cor_matrix_remainingC, cutoff = 0.8)
highly_correlated_varsC <- highly_correlated_varsC[highly_correlated_varsC!=8]

# Remove the highly correlated variables from the data table
my_data_subsetC <- my_data_subsetC[, -highly_correlated_varsC, with = FALSE]
varC <- colnames(my_data_subsetC)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetC)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetC,Communication[,c(2,11)], by = "EPS_Change")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
Communication_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
Communication_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#Modeling for Communication Sector
#Linear model
model_communication <- lm(EPS_Change  ~ ., data = my_data_subsetC)
summary(model_communication)
CV(model_communication)

#Timeseries model with macros
model_C_ts <- tslm(EPS_Change ~ COGS_Growth+EBIT_Growth+Pretax_Income_Growth    
                    +Gross_Margin+EBITDA_Margin+Price_Earnings+Enterprise_Value_EBIT+Dividend_Yield
                    +FEDFUNDS+PERMIT +Recession+annual_m2,data = Communication_Eps_ts1)

#In sample accuracy of model with macros
summary(model_C_ts)
CV(model_C_ts)
x_C <- Communication_Eps_ts2[1:3,varC]
x_C <- as.data.frame(x_C)

#Finding the Forecasted EPS from EPS Growth
forecast_C <- forecast(model_C_ts,newdata = x_C, h = 1)
plot(forecast_C)
Communication_Eps_ts2 <- cbind(Communication_Eps_ts2,forecast_C,EPS_C,EPS_C_shift)
C_Eps_df <- as.data.frame(Communication_Eps_ts2)

C_Eps_df$EPS_fcast[1] <- (1+C_Eps_df$`forecast_C.Point Forecast`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast80h[1] <- (1+C_Eps_df$`forecast_C.Hi 80`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast80l[1] <- (1+C_Eps_df$`forecast_C.Lo 80`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast95h[1] <- (1+C_Eps_df$`forecast_C.Hi 95`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast95l[1] <- (1+C_Eps_df$`forecast_C.Lo 95`[1])*C_Eps_df$EPS_C_shift[1]

for(i in 2:3){
  C_Eps_df$EPS_fcast[i] <- (1+C_Eps_df$`forecast_C.Point Forecast`[i]) *C_Eps_df$EPS_fcast[i-1]
  C_Eps_df$EPS_fcast80h[i] <- (1+C_Eps_df$`forecast_C.Hi 80`[i]) *C_Eps_df$EPS_fcast80h[i-1]
  C_Eps_df$EPS_fcast80l[i] <- (1+C_Eps_df$`forecast_C.Lo 80`[i]) *C_Eps_df$EPS_fcast80l[i-1]
  C_Eps_df$EPS_fcast95h[i] <- (1+C_Eps_df$`forecast_C.Hi 95`[i])*C_Eps_df$EPS_fcast95h[i-1]
  C_Eps_df$EPS_fcast95l[i] <- (1+C_Eps_df$`forecast_C.Lo 95`[i])*C_Eps_df$EPS_fcast95l[i-1]
}

#Finding the out of sample accuracy
C_Eps_df <- cbind(C_Eps_df,Communication$DATE[20:22])
Comm_Macro <-accuracy(C_Eps_df$EPS_fcast,C_Eps_df$EPS_C)

#Finding accuracy of analyst forecast
C_Eps_df$`Communication$DATE[20:22]` <- as.character(C_Eps_df$`Communication$DATE[20:22]` )
C_Eps_df <- merge(C_Eps_df,C_analyst, by.x = "Communication$DATE[20:22]", by.y = "DATE")
Comm_fcast_Macro <- accuracy(as.numeric(C_Eps_df$EPS),C_Eps_df$EPS_C)

#adding to a db
C_fcast <- data.frame(c("Date","EPS","Data_Type"))
C_fcast$Date <-  C_Eps_df$`Communication$DATE[20:22]`
C_fcast$EPS <- C_Eps_df$EPS_fcast
C_fcast$Data_Type <- "Forecast with Macro"

C_anal <- data.frame(c("Date","EPS","Data_Type"))
C_anal$Date <-  C_Eps_df$`Communication$DATE[20:22]`
C_anal$EPS <- C_Eps_df$EPS
C_anal$Data_Type <- "Analyst Forecast"

C_actual <- data.frame(c("Date","EPS","Data_Type"))
C_actual$Date <-  C_Eps_df$`Communication$DATE[20:22]`
C_actual$EPS <- C_Eps_df$EPS_C
C_actual$Data_Type <- "Actual EPS"

#Timeseries model without macros
model_C_ts <- tslm(EPS_Change ~ COGS_Growth+EBIT_Growth+Pretax_Income_Growth    
                   +Gross_Margin+EBITDA_Margin+Price_Earnings+Enterprise_Value_EBIT+Dividend_Yield
                   +Recession,data = Communication_Eps_ts1)

#In sample accuracy of model without macros
summary(model_C_ts)
CV(model_C_ts)
Communication_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

x_C <- Communication_Eps_ts2
x_C <- as.data.frame(x_C)

#Finding the Forecasted EPS from EPS Growth
forecast_C <- forecast(model_C_ts,newdata = x_C, h = 1)
plot(forecast_C)
Communication_Eps_ts2 <- cbind(Communication_Eps_ts2,forecast_C,EPS_C,EPS_C_shift)
C_Eps_df <- as.data.frame(Communication_Eps_ts2)

C_Eps_df$EPS_fcast[1] <- (1+C_Eps_df$`forecast_C.Point Forecast`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast80h[1] <- (1+C_Eps_df$`forecast_C.Hi 80`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast80l[1] <- (1+C_Eps_df$`forecast_C.Lo 80`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast95h[1] <- (1+C_Eps_df$`forecast_C.Hi 95`[1])*C_Eps_df$EPS_C_shift[1]
C_Eps_df$EPS_fcast95l[1] <- (1+C_Eps_df$`forecast_C.Lo 95`[1])*C_Eps_df$EPS_C_shift[1]

for(i in 2:3){
  C_Eps_df$EPS_fcast[i] <- (1+C_Eps_df$`forecast_C.Point Forecast`[i]) *C_Eps_df$EPS_fcast[i-1]
  C_Eps_df$EPS_fcast80h[i] <- (1+C_Eps_df$`forecast_C.Hi 80`[i]) *C_Eps_df$EPS_fcast80h[i-1]
  C_Eps_df$EPS_fcast80l[i] <- (1+C_Eps_df$`forecast_C.Lo 80`[i]) *C_Eps_df$EPS_fcast80l[i-1]
  C_Eps_df$EPS_fcast95h[i] <- (1+C_Eps_df$`forecast_C.Hi 95`[i])*C_Eps_df$EPS_fcast95h[i-1]
  C_Eps_df$EPS_fcast95l[i] <- (1+C_Eps_df$`forecast_C.Lo 95`[i])*C_Eps_df$EPS_fcast95l[i-1]
}

#Out of sample accuracy
C_Eps_df <- cbind(C_Eps_df,Communication$DATE[20:22])
Comm_Macro_N <-accuracy(C_Eps_df$EPS_fcast,C_Eps_df$EPS_C)

#DF
C_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
C_fcast_M$Date <- as.character(C_Eps_df$`Communication$DATE[20:22]`)
C_fcast_M$EPS <- C_Eps_df$EPS_fcast
C_fcast_M$Data_Type <- "Forecast Without Macro"

##Plot
C <- rbind(C_fcast,C_fcast_M,C_anal,C_actual)
C$Date <- as.Date(C$Date)
C$EPS <- as.numeric(C$EPS)
C <- C[,-1]

ggplot(C, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

################################################################################################################
#Modeling LR

#Model 1: Linear Regression
drop_train <- c("Sector","DATE","Net Income Growth","Dividends Growth","COGS Growth")
train_data <- combined_actuals[,!(names(combined_actuals) %in% drop_train)]
model1<- lm(train_data$`EPS Change` ~ .,data = train_data)
summary(model1)

#Model 2: Linear Regression with interactions
model2<- lm(train_data$`EPS Change` ~.^2,data = train_data)
summary(model1)

#Model 3: Panel Model
drop_train <- c("Sector","Net Income Growth","Dividends Growth","COGS Growth")
train_data <- combined_actuals[,!(names(combined_actuals) %in% drop_train)]
ols<-lm(train_data$`EPS Change` ~ ., data=train_data) 
summary(ols)