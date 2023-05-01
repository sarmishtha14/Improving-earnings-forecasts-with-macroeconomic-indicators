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
load("~/Desktop/DUKE/Capstone/factset_forecasts.rda")
combined_forecasts <- combined_forecasts[,-1]
#Energy
Energy_analyst <- combined_forecasts[combined_forecasts$Sector == "Energy",]
Energy_3 <- Energy_analyst[12:14,]
#Information Technology
IT_analyst <- combined_forecasts[combined_forecasts$Sector == "Technology",]
IT_3 <- IT_analyst[12:14,]

#Loading the factset data
load("~/Desktop/DUKE/Capstone/factset_actuals.rda")

#Converting percentage terms to actual terms
combined_actuals[,1:16] <- combined_actuals[,1:16]/100

#Loading the Housing Index data and merge it
setwd("/Users/gmx/Desktop/DUKE/Capstone")
Housing_index <- read.csv("monthlyPERMIT.csv")
Housing_index$DATE <- as.Date(Housing_index$DATE)
combined_actuals$DATE <- as.Date(combined_actuals$DATE)
combined_actuals <- merge(combined_actuals, Housing_index, by.x = "DATE",by.y = "DATE")
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

#Correlation Matrix
library(corrplot)
#data <- combined_actuals[,c(-1,-36:-35)]
#colnames(data) <- c("SRG","COGS","GIG","EBIT","EBITDA","PIG","NIG","DG",
#                    "EPSG","DC","GM","EBITM","EBITDAM","PM","NM","FCFCM","PE","PEX",
#                    "PS","PBV","PCF","PCFX","PVC","EVEBIT","EVEBITDA","EVS",
#                    "ROE","DY","EPS","SPS","DPS","BVS","CVS","FCF","FED","INF",
#                    "RFED","I","HC","CD","IT","E","F","CS","M","RE","U","TC","R")
#M = cor(data[,2:20])
#corrplot(M)

#Adding dummy variable for EPS recession
combined_actuals$Recession <- ifelse(year(combined_actuals$DATE) == 2001 | year(combined_actuals$DATE) == 2008 | year(combined_actuals$DATE) == 2009 | year(combined_actuals$DATE) == 2020 | year(combined_actuals$DATE) == 2017,1,0)
#combined_actuals$Recession <- factor(combined_actuals$Recession)

#EPSG and NIG are highly correlated, so we can remove NIG
#DC and DG are highly correlated, so we can remove DC

#Timeseries/Categorical/Continuous/Discrete

#Information Technology EDA

#InformationTechnology ts
InformationTechnology <- combined_actuals[combined_actuals$Sector=="Technology",]
InformationTechnology<-InformationTechnology[,-(39:50)]
InformationTechnology <- InformationTechnology[month(InformationTechnology$DATE) == 10,]

#Exploratory data analysis
#ggplot(data = InformationTechnology, mapping = aes(x=InformationTechnology$DATE )) +
#  geom_line(mapping = aes(y = InformationTechnology$`COGS Growth`), color = "blue") +
#  geom_line(aes(y = InformationTechnology$`inflation rate`), color = "red") +
#  geom_line(aes(y = InformationTechnology$`EBIT Growth`), color = "green") +
#  geom_line(aes(y = InformationTechnology$`FEDFUNDS`), color = "black") +
#  geom_line(aes(y = InformationTechnology$`EPS Change`), color = "grey") 


#Seasonality
#ggseasonplot(InformationTechnology_Eps_ts )
#ggsubseriesplot(InformationTechnology_Eps_ts)

#Correlations

InformationTechnology <- data.table(InformationTechnology)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetIT <- InformationTechnology[, c("Sales/Revenue Growth","COGS Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
                                        "Net Income Growth", "Dividend Growth", "EPS Change", "Dividends Change", 
                                        "Gross Margin", "EBIT Margin", "EBITDA Margin", "Pretax Margin", 
                                        "Net Margin", "Free Cash Flow Margin", "Price/Earnings", 
                                        "Price/Earnings (x) (excl negatives)", "Price/Sales (x)", "Price/Book Value (x)", 
                                        "Price/Cash Flow (x)", "Price/Cash Flow (x) (excl negatives)", 
                                        "Price/Free Cash Flow (x)", "Enterprise Value/EBIT (x)", 
                                        "Enterprise Value/EBITDA (x)", "Enterprise Value/Sales (x)", 
                                        "Return on Equity (%)", "Dividend Yield (%)", "EPS (recurrent earnings)",
                                        "Sales Per Share", "Dividends Per Share", "Book Value Per Share", 
                                        "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT")]

colnames(InformationTechnology) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                               "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                               "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                               "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                               "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                               "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                               "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                               "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                               "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                               "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","PERMIT","Recession")
colnames(my_data_subsetIT) <- c("Sales_Revenue_Growth","COGS_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                                "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                                "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                                "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                                "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                                "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                                "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                                "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                                "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                                "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                                "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT")

EPS_IT <- InformationTechnology[20:22,"EPS_recurrent_earnings"]
EPS_IT_shift <- InformationTechnology[19:21,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingIT <- cor(as.matrix(my_data_subsetIT))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsIT <- findCorrelation(cor_matrix_remainingIT)
highly_correlated_varsIT <- highly_correlated_varsIT[highly_correlated_varsIT!=9]

# Remove the highly correlated variables from the data table
my_data_subsetIT <- my_data_subsetIT[, -highly_correlated_varsIT, with = FALSE]
varIT <- colnames(my_data_subsetIT)
#varIT <- append(varIT,"EPS")

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetIT)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetIT,InformationTechnology[,c(1,10,40)], by = "EPS_Change")
#CS_analyst$DATE <- as.character(CS_analyst$DATE)
#m1$DATE <- as.character(m1$DATE)
#m1 <- merge(m1,CS_analyst[1:14,c(1,4)],by= "DATE")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
InformationTechnology_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
InformationTechnology_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#the effectiveness of the model will ultimately depend on the quality of the data and the appropriateness of the model assumptions.

#Modeling for InformationTechnology Sector
#Linear model
#The model is being fitted using the "EPS_Change" variable as the response variable and all other variables in the "my_data_subsetIT" dataset as predictors.
model_IT <- lm(EPS_Change  ~ ., data = my_data_subsetIT)
summary(model_IT) ##The "summary" function is then used to provide a summary of the model, which includes information on the coefficients, standard errors, p-values, and R-squared value.
CV(model_IT)  ##Finally, the "CV" function is being used to perform cross-validation on the model. Cross-validation is a technique used to evaluate the performance of a model by testing it on data that was not used in the model fitting process.

#Timeseries model with macros
# fitting a time series linear regression model with macroeconomic variables for the Information Technology sector using the "tslm" function
#The "EPS_Change" variable is being modeled as the response variable, and the predictors include a combination of macroeconomic variables and financial ratios such as "Sales_Revenue_Growth," "COGS_Growth," "EBIT_Growth," "Pretax_Income_Growth," "Dividends_Change," "Price_Earnings," "Price_Earnings_excl," "Enterprise_Value_Sales," and "Sales_Per_Share." Additionally, two macroeconomic variables "inflation_rate" and "PERMIT" are also included in the model.
model_IT_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+COGS_Growth+EBIT_Growth+Pretax_Income_Growth+Dividends_Change    
                    +Price_Earnings+Price_Earnings_excl+Enterprise_Value_Sales+Sales_Per_Share
                    +inflation_rate+PERMIT,data = InformationTechnology_Eps_ts1)

#In sample accuracy of model with macros
summary(model_IT_ts)
CV(model_IT_ts)
x_IT <- InformationTechnology_Eps_ts2[1:3,varIT]  ##Extracting a subset of the data ("InformationTechnology_Eps_ts2") and using it to make predictions with the model. Specifically, they are selecting the first three rows and only the variables in the "varIT" vector. The resulting subset is then converted to a data frame using the "as.data.frame" function.
x_IT <- as.data.frame(x_IT)

#Finding the Forecasted EPS from EPS Growth
forecast_IT <- forecast(model_IT_ts,newdata = x_IT, h = 1)    #using the time series linear regression model ("model_IT_ts") to make a one-step ahead forecast of EPS_Change for the Information Technology sector. The "forecast" function is being used to generate the forecast, with the "newdata" argument set to "x_IT" (which contains the predictor variable values for the forecast period) and the "h" argument set to 1 (indicating a one-step ahead forecast).
plot(forecast_IT)
InformationTechnology_Eps_ts2 <- cbind(InformationTechnology_Eps_ts2,forecast_IT,EPS_IT,EPS_IT_shift) #combining the forecasted values with the actual values of EPS_Change for the Information Technology sector ("EPS_IT" and "EPS_IT_shift") and storing the result in a new data frame called "IT_Eps_df".
IT_Eps_df <- as.data.frame(InformationTechnology_Eps_ts2)

IT_Eps_df$EPS_fcast[1] <- (1+IT_Eps_df$`forecast_IT.Point Forecast`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast80h[1] <- (1+IT_Eps_df$`forecast_IT.Hi 80`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast80l[1] <- (1+IT_Eps_df$`forecast_IT.Lo 80`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast95h[1] <- (1+IT_Eps_df$`forecast_IT.Hi 95`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast95l[1] <- (1+IT_Eps_df$`forecast_IT.Lo 95`[1])*IT_Eps_df$EPS_IT_shift[1]

for(i in 2:3){
  IT_Eps_df$EPS_fcast[i] <- (1+IT_Eps_df$`forecast_IT.Point Forecast`[i]) *IT_Eps_df$EPS_fcast[i-1]
  IT_Eps_df$EPS_fcast80h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 80`[i]) *IT_Eps_df$EPS_fcast80h[i-1]
  IT_Eps_df$EPS_fcast80l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 80`[i]) *IT_Eps_df$EPS_fcast80l[i-1]
  IT_Eps_df$EPS_fcast95h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 95`[i])*IT_Eps_df$EPS_fcast95h[i-1]
  IT_Eps_df$EPS_fcast95l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 95`[i])*IT_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of InformationTechnology with Macro
#evaluating the accuracy of the one-step ahead EPS_Change forecast generated using the time series linear regression model with macroeconomic variables for the Information Technology sector.
IT_Eps_df <- cbind(IT_Eps_df,InformationTechnology$DATE[20:22]) #combining the forecasted and actual EPS_Change values with the corresponding dates from the original dataset. The resulting data frame is stored in a new variable called "IT_Eps_df."
ConStap_Macro <- accuracy(IT_Eps_df$EPS_fcast,IT_Eps_df$EPS_IT) #the "accuracy" function is being used to calculate the accuracy measures of the forecast. The "EPS_fcast" and "EPS_IT" columns in the "IT_Eps_df" data frame are being used as the forecasted and actual values, respectively. The resulting accuracy measures are stored in a new variable called "ConStap_Macro."

#Accuracy of InformationTechnology forecast
IT_Eps_df$`InformationTechnology$DATE[20:22]` <- as.character(IT_Eps_df$`InformationTechnology$DATE[20:22]` ) 
IT_Eps_df <- merge(IT_Eps_df,IT_analyst, by.x = "InformationTechnology$DATE[20:22]", by.y = "DATE") #converting the date column in the "IT_Eps_df" data frame to a character vector and merging it with another data frame ("IT_analyst") on the date column. This is likely being done to compare the forecasted EPS_Change values with analyst EPS_Change estimates.
ConStap_fcast_Macro_ <- accuracy(as.numeric(IT_Eps_df$EPS),IT_Eps_df$EPS_IT) #the "accuracy" function is being used to calculate the accuracy measures of the forecast. The "EPS" and "EPS_IT" columns in the "IT_Eps_df" data frame are being used as the forecasted and actual values, respectively. The resulting accuracy measures are stored in a new variable called "ConStap_fcast_Macro_".

#adding to a db
#first creating an empty data frame with three columns ("Date", "EPS", and "Data_Type") using the "data.frame" function. The "Date" column is then populated with the dates from the "InformationTechnology$DATE[20:22]" column of the "IT_Eps_df" data frame. The "EPS" column is populated with the forecasted, analyst, or actual EPS values from the "EPS_fcast", "EPS", or "EPS_IT" columns of the "IT_Eps_df" data frame, respectively. Finally, the "Data_Type" column is set to "Forecast", "Analyst", or "Actual" to indicate the source of the EPS value.
IT_fcast <- data.frame(c("Date","EPS","Data_Type"))
IT_fcast$Date <-  IT_Eps_df$`InformationTechnology$DATE[20:22]`
IT_fcast$EPS <- IT_Eps_df$EPS_fcast
IT_fcast$Data_Type <- "Forecast"

IT_anal <- data.frame(c("Date","EPS","Data_Type"))
IT_anal$Date <-  IT_Eps_df$`InformationTechnology$DATE[20:22]`
IT_anal$EPS <- IT_Eps_df$EPS
IT_anal$Data_Type <- "Analyst"

IT_actual <- data.frame(c("Date","EPS","Data_Type"))
IT_actual$Date <-  IT_Eps_df$`InformationTechnology$DATE[20:22]`
IT_actual$EPS <- IT_Eps_df$EPS_IT
IT_actual$Data_Type <- "Actual"


####################Timeseries model without macros
#fitting a time series linear regression model to the "InformationTechnology_Eps_ts1" data using the "tslm" function. The dependent variable ("EPS_Change") is regressed on a number of independent variables that are listed in the formula using the "~" symbol. These independent variables include "Sales_Revenue_Growth", "COGS_Growth", "EBIT_Growth", "Pretax_Income_Growth", "Dividends_Change", "Price_Earnings", "Price_Earnings_excl", "Enterprise_Value_Sales", "Sales_Per_Share", and "PERMIT".
#The model appears to be intended for the Information Technology sector, given the name of the dataset. The independent variables listed are commonly used financial metrics to analyze companies and sectors, such as revenue growth, earnings before interest and taxes (EBIT) growth, and price-to-earnings ratio. The inclusion of these variables suggests that the model is trying to capture the relationships between these metrics and the EPS of the Information Technology sector over time.
#performing a regression analysis, which is a common statistical method used to explore the relationship between variables
model_IT_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+COGS_Growth+EBIT_Growth+Pretax_Income_Growth+Dividends_Change    
                   +Price_Earnings+Price_Earnings_excl+Enterprise_Value_Sales+Sales_Per_Share
                    +PERMIT,data = InformationTechnology_Eps_ts1)

#In sample accuract of model without macros
#The model includes multiple independent variables such as Sales Revenue Growth, COGS Growth, EBIT Growth, Pretax Income Growth, Dividends Change, Price Earnings, Price Earnings Excl, Enterprise Value Sales, Sales Per Share, and PERMIT. The dependent variable in the model is EPS Change, which is being predicted using the independent variables.
summary(model_IT_ts)
CV(model_IT_ts) #uses the summary() function to obtain a summary of the model, which provides information on the coefficients of the independent variables and the overall fit of the model. The CV() function is also used to perform cross-validation on the model, which can help assess the out-of-sample performance of the model.
varIT <- varIT[-c(16:18)]
InformationTechnology_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
x_IT <- InformationTechnology_Eps_ts2[,varIT] #creates a new data frame x_IT using the InformationTechnology_Eps_ts2 time series data, excluding the last three observations (which were removed in the previous line of code). The x_IT data frame is then used as input for making one-step-ahead forecasts using the forecast() function, which is not shown in the provided code.
x_IT <- as.data.frame(x_IT)

#Finding the Forecasted EPS from EPS Growth
forecast_IT <- forecast(model_IT_ts,newdata = x_IT, h = 1)
plot(forecast_IT)
InformationTechnology_Eps_ts2 <- cbind(InformationTechnology_Eps_ts2,forecast_IT,EPS_IT,EPS_IT_shift)
IT_Eps_df <- as.data.frame(InformationTechnology_Eps_ts2)

IT_Eps_df$EPS_fcast[1] <- (1+IT_Eps_df$`forecast_IT.Point Forecast`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast80h[1] <- (1+IT_Eps_df$`forecast_IT.Hi 80`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast80l[1] <- (1+IT_Eps_df$`forecast_IT.Lo 80`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast95h[1] <- (1+IT_Eps_df$`forecast_IT.Hi 95`[1])*IT_Eps_df$EPS_IT_shift[1]
IT_Eps_df$EPS_fcast95l[1] <- (1+IT_Eps_df$`forecast_IT.Lo 95`[1])*IT_Eps_df$EPS_IT_shift[1]

for(i in 2:3){
  IT_Eps_df$EPS_fcast[i] <- (1+IT_Eps_df$`forecast_IT.Point Forecast`[i]) *IT_Eps_df$EPS_fcast[i-1]
  IT_Eps_df$EPS_fcast80h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 80`[i]) *IT_Eps_df$EPS_fcast80h[i-1]
  IT_Eps_df$EPS_fcast80l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 80`[i]) *IT_Eps_df$EPS_fcast80l[i-1]
  IT_Eps_df$EPS_fcast95h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 95`[i])*IT_Eps_df$EPS_fcast95h[i-1]
  IT_Eps_df$EPS_fcast95l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 95`[i])*IT_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of InformationTechnology without Macro
IT_Eps_df <- cbind(IT_Eps_df,InformationTechnology$DATE[20:22])
ConStap_Macro_N <- accuracy(as.numeric(IT_Eps_df$EPS_fcast),IT_Eps_df$EPS_IT)

#DF
IT_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
IT_fcast_M$Date <- as.character(IT_Eps_df$`InformationTechnology$DATE[20:22]`)
IT_fcast_M$EPS <- IT_Eps_df$EPS_fcast
IT_fcast_M$Data_Type <- "Without Macro"

##plot
IT <- rbind(IT_fcast,IT_fcast_M,IT_anal,IT_actual)
IT$Date <- as.Date(IT$Date)
IT$EPS <- as.numeric(IT$EPS)
IT <- IT[,-1]

ggplot(IT, aes(x = Date, y = EPS, color = Data_Type)) +
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
for(i in 2:3){
  IT_Eps_df$EPS_fcast[i] <- (1+IT_Eps_df$`forecast_IT.Point Forecast`[i]) *IT_Eps_df$EPS_fcast[i-1]
  IT_Eps_df$EPS_fcast80h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 80`[i]) *IT_Eps_df$EPS_fcast80h[i-1]
  IT_Eps_df$EPS_fcast80l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 80`[i]) *IT_Eps_df$EPS_fcast80l[i-1]
  IT_Eps_df$EPS_fcast95h[i] <- (1+IT_Eps_df$`forecast_IT.Hi 95`[i])*IT_Eps_df$EPS_fcast95h[i-1]
  IT_Eps_df$EPS_fcast95l[i] <- (1+IT_Eps_df$`forecast_IT.Lo 95`[i])*IT_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of InformationTechnology without Macro
IT_Eps_df <- cbind(IT_Eps_df,InformationTechnology$DATE[20:22])
ConStap_Macro_N <- accuracy(as.numeric(IT_Eps_df$EPS_fcast),IT_Eps_df$EPS_IT)

#DF
IT_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
IT_fcast_M$Date <- as.character(IT_Eps_df$`InformationTechnology$DATE[20:22]`)
IT_fcast_M$EPS <- IT_Eps_df$EPS_fcast
IT_fcast_M$Data_Type <- "Without Macro"

##plot
IT <- rbind(IT_fcast,IT_fcast_M,IT_anal,IT_actual)
IT$Date <- as.Date(IT$Date)
IT$EPS <- as.numeric(IT$EPS)
IT <- IT[,-1]

ggplot(IT, aes(x = Date, y = EPS, color = Data_Type)) +
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





#################################################################
#Energy Sector EDA

#Energy Industry
Energy <- combined_actuals[combined_actuals$Sector=="Energy",]
Energy<-Energy[,-(39:50)]
Energy$DATE <- as.Date(Energy$DATE)
Energy <- Energy[month(Energy$DATE) == 10,]

#Exploratory data analysis
ggplot(data = Energy, mapping = aes(x=Energy$DATE )) +
  geom_line(mapping = aes(y = Energy$`COGS Growth`), color = "blue") +
  geom_line(aes(y = Energy$`inflation rate`), color = "red") +
  geom_line(aes(y = Energy$`EBIT Growth`), color = "green") +
  geom_line(aes(y = Energy$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = Energy$`EPS Change`), color = "grey") 

#EPS Spikes in the years 2003-2004, 2011-2012, 2018 and 2022
#EPS Steeps in the years 2008-2009 and 2020 (recession)
#EBIT Growth shows similar trend to EPS

#Correlations

Energy <- data.table(Energy)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetE <- Energy[, c("Sales/Revenue Growth","COGS Growth","Gross Income Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
                                 "Net Income Growth", "Dividend Growth", "EPS Change", "Dividends Change", 
                                 "Gross Margin", "EBIT Margin", "EBITDA Margin", "Pretax Margin", 
                                 "Net Margin", "Free Cash Flow Margin", "Price/Earnings", 
                                 "Price/Earnings (x) (excl negatives)", "Price/Sales (x)", "Price/Book Value (x)", 
                                 "Price/Cash Flow (x)", "Price/Cash Flow (x) (excl negatives)", 
                                 "Price/Free Cash Flow (x)", "Enterprise Value/EBIT (x)", 
                                 "Enterprise Value/EBITDA (x)", "Enterprise Value/Sales (x)", 
                                 "Return on Equity (%)", "Dividend Yield (%)", "EPS (recurrent earnings)",
                                 "Sales Per Share", "Dividends Per Share", "Book Value Per Share", 
                                 "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT")]
colnames(Energy) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                         "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                         "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                         "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                         "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                         "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                         "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                         "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                         "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                         "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                         "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","PERMIT","Recession")
colnames(my_data_subsetE) <- c("Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                               "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                               "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                               "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                               "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                               "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                               "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                               "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                               "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                               "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT")

EPS_E <- Energy[20:22,"EPS_recurrent_earnings"]
EPS_E_shift <- Energy[19:21,"EPS_recurrent_earnings"]


# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingE <- cor(as.matrix(na.omit(my_data_subsetE)))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsE <- findCorrelation(cor_matrix_remainingE)
highly_correlated_varsE <- highly_correlated_varsE[highly_correlated_varsE!=9]
# Remove the highly correlated variables from the data table
my_data_subsetE <- my_data_subsetE[, -highly_correlated_varsE, with = FALSE]
varE <- colnames(my_data_subsetE)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetE)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetE,Energy[,c(1,10)], by = "EPS_Change")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
Energy_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
Energy_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)


#Modeling for Energy Sector
#Linear model
#The linear model model_energy is using lm() function to fit a linear regression model on the my_data_subsetE dataset. The dependent variable is EPS_Change and all other columns in the dataset are considered as independent variables. The summary() function is then used to print the summary of the model including the coefficients, standard errors, t-values, and p-values. The CV() function is used to perform cross-validation on the model.
model_energy <- lm(EPS_Change  ~ ., data = my_data_subsetE)
summary(model_energy)
CV(model_energy)

#Timeseries model with macros
#The time series model model_E_ts is using tslm() function to fit a linear regression model on the Energy_Eps_ts1 dataset. The dependent variable is EPS_Change and the independent variables include Sales_Revenue_Growth, EBIT_Growth, Dividends_Change, Pretax_Margin, Free_Cash_Flow_Margin, EPS_recurrent_earnings, Book_Value_Per_Share, Price_Book_Value, Price_Free_Cash_Flow, and Sales_Per_Share. The forecast() function can be used to make predictions using this model.
model_E_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+EBIT_Growth+Dividends_Change     
                  +Pretax_Margin+Free_Cash_Flow_Margin+EPS_recurrent_earnings+Book_Value_Per_Share   
                   +Price_Book_Value + Price_Free_Cash_Flow+Sales_Per_Share,data = Energy_Eps_ts1)

#In sample accuracy of model with macros
summary(model_E_ts) #linear model (lm) using all the variables in my_data_subsetE to predict the EPS_Change. It then provides a summary of the model and cross-validation results.
CV(model_E_ts)
x_E <- Energy_Eps_ts2[1:3,varE] # time series linear model (tslm) using a subset of variables to predict the EPS_Change. The model is fitted to the Energy_Eps_ts1 time series data and then evaluated using summary and cross-validation. x_E is created as a subset of variables from the Energy_Eps_ts2 time series data, which will be used to make forecasts using the fitted model.
x_E <- as.data.frame(x_E)

#Finding the Forecasted EPS from EPS Growth
forecast_E <- forecast(model_E_ts,newdata = x_E, h = 1)
plot(forecast_E)
Energy_Eps_ts2 <- cbind(Energy_Eps_ts2,forecast_E,EPS_E,EPS_E_shift)
E_Eps_df <- as.data.frame(Energy_Eps_ts2)

E_Eps_df$EPS_fcast[1] <- (1+E_Eps_df$`forecast_E.Point Forecast`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast80h[1] <- (1+E_Eps_df$`forecast_E.Hi 80`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast80l[1] <- (1+E_Eps_df$`forecast_E.Lo 80`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast95h[1] <- (1+E_Eps_df$`forecast_E.Hi 95`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast95l[1] <- (1+E_Eps_df$`forecast_E.Lo 95`[1])*E_Eps_df$EPS_E_shift[1]

for(i in 2:3){
  E_Eps_df$EPS_fcast[i] <- (1+E_Eps_df$`forecast_E.Point Forecast`[i]) *E_Eps_df$EPS_fcast[i-1]
  E_Eps_df$EPS_fcast80h[i] <- (1+E_Eps_df$`forecast_E.Hi 80`[i]) *E_Eps_df$EPS_fcast80h[i-1]
  E_Eps_df$EPS_fcast80l[i] <- (1+E_Eps_df$`forecast_E.Lo 80`[i]) *E_Eps_df$EPS_fcast80l[i-1]
  E_Eps_df$EPS_fcast95h[i] <- (1+E_Eps_df$`forecast_E.Hi 95`[i])*E_Eps_df$EPS_fcast95h[i-1]
  E_Eps_df$EPS_fcast95l[i] <- (1+E_Eps_df$`forecast_E.Lo 95`[i])*E_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Energy with Macro
#the forecasted EPS values are added to the E_Eps_df data frame using the cbind() function. Then, the accuracy() function is used to calculate the accuracy of the forecasted EPS values (EPS_fcast) compared to the actual EPS values (EPS_E) for the Energy sector with macroeconomic variables. The results are stored in the ConStap_Macro object.
E_Eps_df <- cbind(E_Eps_df,Energy$DATE[20:22])
ConStap_Macro <- accuracy(E_Eps_df$EPS_fcast,E_Eps_df$EPS_E)

#Accuracy of Energy forecast
#The merge() function is used to combine the E_Eps_df data frame with the Energy_analyst data frame on the "DATE" column, and the accuracy() function is used to calculate the accuracy of the forecasted EPS values (EPS) compared to the actual EPS values (EPS_E) for the Energy sector without macroeconomic variables. The results are stored in the ConStap_fcast_Macro_ object.
E_Eps_df$`Energy$DATE[20:22]` <- as.character(E_Eps_df$`Energy$DATE[20:22]` )
E_Eps_df <- merge(E_Eps_df,Energy_analyst, by.x = "Energy$DATE[20:22]", by.y = "DATE")
ConStap_fcast_Macro_ <- accuracy(as.numeric(E_Eps_df$EPS),E_Eps_df$EPS_E)

#adding to a db
#creating data frames E_fcast, E_anal, and E_actual which will store the forecasted EPS, analyst EPS estimates, and actual EPS values for the Energy sector respectively. The data frames are populated with three columns: Date, EPS, and Data_Type. The Date column is populated with the corresponding dates from the Energy$DATE[20:22] vector, the EPS column is populated with the forecasted, analyst, and actual EPS values respectively, and the Data_Type column is populated with the corresponding data type i.e. "Forecast", "Analyst", or "Actual". 
E_fcast <- data.frame(c("Date","EPS","Data_Type"))
E_fcast$Date <-  E_Eps_df$`Energy$DATE[20:22]`
E_fcast$EPS <- E_Eps_df$EPS_fcast
E_fcast$Data_Type <- "Forecast"

E_anal <- data.frame(c("Date","EPS","Data_Type"))
E_anal$Date <-  E_Eps_df$`Energy$DATE[20:22]`
E_anal$EPS <- E_Eps_df$EPS
E_anal$Data_Type <- "Analyst"

E_actual <- data.frame(c("Date","EPS","Data_Type"))
E_actual$Date <-  E_Eps_df$`Energy$DATE[20:22]`
E_actual$EPS <- E_Eps_df$EPS_E
E_actual$Data_Type <- "Actual"


####################Timeseries model wEhout macros
#fits a linear regression model to the EPS_Change variable in the my_data_subsetE data frame, using all the other variables in the data frame as predictors. It then prints a summary of the model and performs cross-validation on the model
model_E_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth+EBIT_Growth+Dividends_Change     
                   +Pretax_Margin+Free_Cash_Flow_Margin+EPS_recurrent_earnings   
                   +Price_Book_Value + Price_Free_Cash_Flow,data = Energy_Eps_ts1) #fits a time-series linear regression model (tslm) to the EPS_Change variable in the Energy_Eps_ts1 time series data, using several macroeconomic variables as predictors. 
#In sample accuract of model wEhout macros
summary(model_E_ts) 
CV(model_E_ts) #prints a summary of the model and performs cross-validation on the model.
varE <- varE[-c(16:18)]
Energy_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
x_E <- Energy_Eps_ts2[,varE]
x_E <- as.data.frame(x_E)

#Finding the Forecasted EPS from EPS Growth
forecast_E <- forecast(model_E_ts,newdata = x_E, h = 1)
plot(forecast_E)
Energy_Eps_ts2 <- cbind(Energy_Eps_ts2,forecast_E,EPS_E,EPS_E_shift)
E_Eps_df <- as.data.frame(Energy_Eps_ts2)

E_Eps_df$EPS_fcast[1] <- (1+E_Eps_df$`forecast_E.Point Forecast`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast80h[1] <- (1+E_Eps_df$`forecast_E.Hi 80`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast80l[1] <- (1+E_Eps_df$`forecast_E.Lo 80`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast95h[1] <- (1+E_Eps_df$`forecast_E.Hi 95`[1])*E_Eps_df$EPS_E_shift[1]
E_Eps_df$EPS_fcast95l[1] <- (1+E_Eps_df$`forecast_E.Lo 95`[1])*E_Eps_df$EPS_E_shift[1]

for(i in 2:3){
  E_Eps_df$EPS_fcast[i] <- (1+E_Eps_df$`forecast_E.Point Forecast`[i]) *E_Eps_df$EPS_fcast[i-1]
  E_Eps_df$EPS_fcast80h[i] <- (1+E_Eps_df$`forecast_E.Hi 80`[i]) *E_Eps_df$EPS_fcast80h[i-1]
  E_Eps_df$EPS_fcast80l[i] <- (1+E_Eps_df$`forecast_E.Lo 80`[i]) *E_Eps_df$EPS_fcast80l[i-1]
  E_Eps_df$EPS_fcast95h[i] <- (1+E_Eps_df$`forecast_E.Hi 95`[i])*E_Eps_df$EPS_fcast95h[i-1]
  E_Eps_df$EPS_fcast95l[i] <- (1+E_Eps_df$`forecast_E.Lo 95`[i])*E_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Energy wEhout Macro
E_Eps_df <- cbind(E_Eps_df,Energy$DATE[20:22])
ConStap_Macro_N <- accuracy(as.numeric(E_Eps_df$EPS_fcast),E_Eps_df$EPS_E)

#DF
E_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
E_fcast_M$Date <- as.character(E_Eps_df$`Energy$DATE[20:22]`)
E_fcast_M$EPS <- E_Eps_df$EPS_fcast
E_fcast_M$Data_Type <- "WEhout Macro"

##plot
E <- rbind(E_fcast,E_fcast_M,E_anal,E_actual)
E$Date <- as.Date(E$Date)
E$EPS <- as.numeric(E$EPS)
E <- E[,-1]


ggplot(E, aes(x = Date, y = EPS, color = Data_Type)) +
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
for(i in 2:3){
  E_Eps_df$EPS_fcast[i] <- (1+E_Eps_df$`forecast_E.Point Forecast`[i]) *E_Eps_df$EPS_fcast[i-1]
  E_Eps_df$EPS_fcast80h[i] <- (1+E_Eps_df$`forecast_E.Hi 80`[i]) *E_Eps_df$EPS_fcast80h[i-1]
  E_Eps_df$EPS_fcast80l[i] <- (1+E_Eps_df$`forecast_E.Lo 80`[i]) *E_Eps_df$EPS_fcast80l[i-1]
  E_Eps_df$EPS_fcast95h[i] <- (1+E_Eps_df$`forecast_E.Hi 95`[i])*E_Eps_df$EPS_fcast95h[i-1]
  E_Eps_df$EPS_fcast95l[i] <- (1+E_Eps_df$`forecast_E.Lo 95`[i])*E_Eps_df$EPS_fcast95l[i-1]
}

#Accuracy of Energy wEhout Macro
E_Eps_df <- cbind(E_Eps_df,Energy$DATE[20:22])
ConStap_Macro_N <- accuracy(as.numeric(E_Eps_df$EPS_fcast),E_Eps_df$EPS_E)

#DF
E_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
E_fcast_M$Date <- as.character(E_Eps_df$`Energy$DATE[20:22]`)
E_fcast_M$EPS <- E_Eps_df$EPS_fcast
E_fcast_M$Data_Type <- "WEhout Macro"

##plot
E <- rbind(E_fcast,E_fcast_M,E_anal,E_actual)
E$Date <- as.Date(E$Date)
E$EPS <- as.numeric(E$EPS)
E <- E[,-1]

ggplot(E, aes(x = Date, y = EPS, color = Data_Type)) +
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
