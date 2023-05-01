#library the required package
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
library(quantmod)

#Loading the forecast factset data
load("/Users/zhangwy/Desktop/Capstone Project/factset_forecasts.rda")
combined_forecasts <- combined_forecasts[,-1]

#analyst's forecast on Healthcare sector
Healthcare_analyst <- combined_forecasts[combined_forecasts$Sector == "Healthcare",]
Healthcare_3 <- Healthcare_analyst[12:14,]

#analyst's forecast on Consumer Discretionary
CD_analyst <- combined_forecasts[combined_forecasts$Sector == "Consumer Discretionary",]
CD_3 <- CD_analyst[12:14,]

#Loading the factset data
load("/Users/zhangwy/Desktop/Capstone Project/factset_actuals.rda")

#Converting percentage terms to actual terms
combined_actuals[,1:16] <- combined_actuals[,1:16]/100

#Loading the macro Index data and merge it
Housing_index <- read.csv("/Users/zhangwy/Desktop/Capstone Project/monthlyPERMIT.csv")
Housing_index$DATE <- as.Date(Housing_index$DATE)
combined_actuals$DATE <- as.Date(combined_actuals$DATE)
combined_actuals <- merge(combined_actuals, Housing_index, by.x = "DATE",by.y = "DATE")

#fixed the na value
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
data <- combined_actuals[,-36:-35]
colnames(data) <- c("SRG","COGS","GIG","EBIT","EBITDA","PIG","NIG","DG",
                    "EPSG","DC","GM","EBITM","EBITDAM","PM","NM","FCFCM","PE","PEX",
                    "PS","PBV","PCF","PCFX","PVC","EVEBIT","EVEBITDA","EVS",
                    "ROE","DY","EPS","SPS","DPS","BVS","CVS","FCF","FED","INF",
                    "RFED","I","HC","CD","IT","E","F","CS","M","RE","U","TC","R")
M = cor(data[,2:20])
corrplot(M)

#Adding dummy variable for EPS recession
combined_actuals$Recession <- ifelse(year(combined_actuals$DATE) == 2001 | year(combined_actuals$DATE) == 2008 | year(combined_actuals$DATE) == 2009 | year(combined_actuals$DATE) == 2020 | year(combined_actuals$DATE) == 2017,1,0)
#combined_actuals$Recession <- factor(combined_actuals$Recession)

#EPSG and NIG are highly correlated, so we can remove NIG
#DC and DG are highly correlated, so we can remove DC

#Timeseries/Categorical/Continuous/Discrete

#Healthcare ts
Healthcare <- combined_actuals[combined_actuals$Sector=="Healthcare",]

#Remove the dummy variable column
Healthcare <- Healthcare[,-(39:50)]

#change to the yearly data
Healthcare <- Healthcare[month(Healthcare$DATE) == 10,]

#Exploratory data analysis
ggplot(data = Healthcare, mapping = aes(x=Healthcare$DATE )) +
  geom_line(mapping = aes(y = Healthcare$`COGS Growth`), color = "blue") +
  geom_line(aes(y = Healthcare$`inflation rate`), color = "red") +
  geom_line(aes(y = Healthcare$`EBIT Growth`), color = "green") +
  geom_line(aes(y = Healthcare$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = Healthcare$`EPS Change`), color = "grey") 
#Only COG Growth has significant fluctuation in Recession period


#Seasonality of the Healthcare Sector
Healthcare_Eps_ts <- ts(Healthcare$`EPS Change`, start = c(2001, 1), end = c(2023, 1), frequency = 4)
ggseasonplot(Healthcare_Eps_ts)
ggsubseriesplot(Healthcare_Eps_ts)

#change it to the data table
Healthcare <- data.table(Healthcare)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetHC <- Healthcare[, c("Sales/Revenue Growth","COGS Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
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

#Rename the column names
colnames(Healthcare) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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
colnames(my_data_subsetHC) <- c("Sales_Revenue_Growth","COGS_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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

#Get the real EPS for valuating the model
EPS_HC <- Healthcare[20:22,"EPS_recurrent_earnings"]
EPS_HC_shift <- Healthcare[19:21,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingHC <- cor(as.matrix(my_data_subsetHC))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsHC <- findCorrelation(cor_matrix_remainingHC)
highly_correlated_varsHC <- highly_correlated_varsHC[highly_correlated_varsHC!=9]

# Remove the highly correlated variables from the data table
my_data_subsetHC <- my_data_subsetHC[, -highly_correlated_varsHC, with = FALSE]
varHC <- colnames(my_data_subsetHC)


# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetHC)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetHC,Healthcare[,c(1,10,40)], by = "EPS_Change")

#Change the data set to the time-series data set
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])

#train data
Healthcare_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)

#test data
Healthcare_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#Consumer Discretionary Sector
ConsumerD <- combined_actuals[combined_actuals$Sector=="Consumer Discretionary",]

#Remove all the dummy variables
ConsumerD <- ConsumerD[,-(39:50)]
ConsumerD$DATE <- as.Date(ConsumerD$DATE)

#Using annually data
ConsumerD <- ConsumerD[month(ConsumerD$DATE) == 10,]

#Exploratory data analysis
ggplot(data = ConsumerD, mapping = aes(x=ConsumerD$DATE )) +
  geom_line(mapping = aes(y = ConsumerD$`COGS Growth`), color = "blue") +
  geom_line(aes(y = ConsumerD$`inflation rate`), color = "red") +
  geom_line(aes(y = ConsumerD$`EBIT Growth`), color = "green") +
  geom_line(aes(y = ConsumerD$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = ConsumerD$`EPS Change`), color = "grey") 

#COGS Growth fluctuates during Recession period
#EBIT Growth shows similar trend to EPS

#change it to data table

ConsumerD <- data.table(ConsumerD)

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetCD <- ConsumerD[, c("Sales/Revenue Growth","COGS Growth","Gross Income Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
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

#Rename the column names
colnames(ConsumerD) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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
colnames(my_data_subsetCD) <- c("Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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

#Get the real ERS 
EPS_ConsumerD <- ConsumerD[20:22,"EPS_recurrent_earnings"]
EPS_ConsumerD_shift <- ConsumerD[19:21,"EPS_recurrent_earnings"]


# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingCD <- cor(as.matrix(my_data_subsetCD))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsCD <- findCorrelation(cor_matrix_remainingCD, cutoff = 0.8)
highly_correlated_varsCD <- highly_correlated_varsCD[highly_correlated_varsCD!=9]

# Remove the highly correlated variables from the data table
my_data_subsetCD <- my_data_subsetCD[, -highly_correlated_varsCD, with = FALSE]
varCD <- colnames(my_data_subsetCD)


# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetCD)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Changing to time-series dataset
m1 <- merge(my_data_subsetCD, ConsumerD[,c(1,10,40)], by = "EPS_Change")
m1 <- m1[order(DATE)]
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])

#train data
ConsumerD_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)

#test data
ConsumerD_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

#Seasonality
autoplot(ConsumerD_Eps_ts1)

#Modeling

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

#Modeling for Consumer Discrementary Sector
#Linear model1
my_data_subsetCD1 <- my_data_subsetCD[,-4]
model_ConsumerD1 <- lm(EPS_Change~ ., data = my_data_subsetCD1)
summary(model_ConsumerD1)
CV(model_ConsumerD1)
#Not a good model with many variables not significant

#Linear Model 2 (Only include varibles with good p-value)
model_ConsumerD2 <- lm(EPS_Change ~ EBIT_Growth + Dividends_Change + Price_Earnings
                        + inflation_rate, data = my_data_subsetCD1)
summary(model_ConsumerD2)
CV(model_ConsumerD2)
#Not a good model with low R square

#Timeseries model with Macro
model_ConsumerD_ts <- tslm(EPS_Change ~ COGS_Growth + EBIT_Growth + Pretax_Income_Growth +  
                             Dividends_Change + EBITDA_Margin + Price_Earnings + Price_Free_Cash_Flow + 
                             Enterprise_Value_EBIT + Dividend_Yield + Sales_Per_Share 
                           + Free_Cash_Flow_Per_Share + inflation_rate + Recession, data = ConsumerD_Eps_ts1)
summary(model_ConsumerD_ts)
CV(model_ConsumerD_ts)

#Get the real data
varCD <- append(varCD,"Recession")
x_CD <- ConsumerD_Eps_ts2[1:3,varCD]
x_CD <- as.data.frame(x_CD)

#Using TSLM model to do the forecast
forecast_CD <- forecast(model_ConsumerD_ts,newdata = x_CD, h = 1)

#Get the forecast plot
plot(forecast_CD)

#Combine the forecast data and the real data
ConsumerD_Eps_ts2 <- cbind(ConsumerD_Eps_ts2,forecast_CD,EPS_ConsumerD,EPS_ConsumerD_shift)
ConsumerD_Eps_df <- as.data.frame(ConsumerD_Eps_ts2)

#Compute the real EPS using EPS_Growth
ConsumerD_Eps_df$EPS_fcast[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Point Forecast`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast80h[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 80`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast80l[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 80`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast95h[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 95`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast95l[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 95`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]

for(i in 2:3){
  ConsumerD_Eps_df$EPS_fcast[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Point Forecast`[i]) *ConsumerD_Eps_df$EPS_fcast[i-1]
  ConsumerD_Eps_df$EPS_fcast80h[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 80`[i]) *ConsumerD_Eps_df$EPS_fcast80h[i-1]
  ConsumerD_Eps_df$EPS_fcast80l[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 80`[i]) *ConsumerD_Eps_df$EPS_fcast80l[i-1]
  ConsumerD_Eps_df$EPS_fcast95h[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 95`[i])*ConsumerD_Eps_df$EPS_fcast95h[i-1]
  ConsumerD_Eps_df$EPS_fcast95l[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 95`[i])*ConsumerD_Eps_df$EPS_fcast95l[i-1]
}

#Combine the data
ConsumerD_Eps_df <- cbind(ConsumerD_Eps_df,ConsumerD$DATE[20:22])

#Valuating the data using accuracy funtion
CD_macro <- accuracy(ConsumerD_Eps_df$EPS_fcast, ConsumerD_Eps_df$EPS_ConsumerD)
#Seems having a big mean absolute error with actual data

#The analyst's forecast
ConsumerD_Eps_df$`ConsumerD$DATE[20:22]` <- as.character(ConsumerD_Eps_df$`ConsumerD$DATE[20:22]` )
CD_3$DATE <- as.character(c("2020-10-01", "2021-10-01", "2022-10-01"))
ConsumerD_Eps_df <- merge(ConsumerD_Eps_df,CD_3, by.x =  "ConsumerD$DATE[20:22]", by.y = "DATE")
CD_Macro_fcast <-accuracy(as.numeric(ConsumerD_Eps_df$EPS),ConsumerD_Eps_df$EPS_ConsumerD)
#Analyst's did better

#create the dataset for plotting
CD_fcast <- data.frame(c("Date","EPS","Data_Type"))
CD_fcast$Date <-  ConsumerD_Eps_df$`ConsumerD$DATE[20:22]`
CD_fcast$EPS <- ConsumerD_Eps_df$EPS_fcast
CD_fcast$Data_Type <- "Forecast with Macro"

CD_analyst <- data.frame(c("Date","EPS","Data_Type"))
CD_analyst$Date <-  ConsumerD_Eps_df$`ConsumerD$DATE[20:22]`
CD_analyst$EPS <- ConsumerD_Eps_df$EPS
CD_analyst$Data_Type <- "Analyst Forecast"

CD_actual <- data.frame(c("Date","EPS","Data_Type"))
CD_actual$Date <-  ConsumerD_Eps_df$`ConsumerD$DATE[20:22]`
CD_actual$EPS <- ConsumerD_Eps_df$EPS_ConsumerD
CD_actual$Data_Type <- "Actual EPS"

#model without macro data
model_ConsumerD_ts <- tslm(EPS_Change ~ COGS_Growth + EBIT_Growth + Pretax_Income_Growth +  
                             Dividends_Change + EBITDA_Margin  + Price_Free_Cash_Flow + 
                             Enterprise_Value_EBIT + Dividend_Yield + Sales_Per_Share  
                           + Free_Cash_Flow_Per_Share, data = ConsumerD_Eps_ts1)
summary(model_ConsumerD_ts)
CV(model_ConsumerD_ts)
#Better than the previous model with macro data

#Making the forecast
forecast_CD <- forecast(model_ConsumerD_ts,newdata = x_CD, h = 1)
plot(forecast_CD)

#Combine the dataset with the real data
ConsumerD_Eps_ts2 <- cbind(ConsumerD_Eps_ts2,forecast_CD,EPS_ConsumerD,EPS_ConsumerD_shift)
ConsumerD_Eps_df <- as.data.frame(ConsumerD_Eps_ts2)

#Calculating the real EPS using EPS Growth
ConsumerD_Eps_df$EPS_fcast[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Point Forecast`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast80h[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 80`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast80l[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 80`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast95h[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 95`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]
ConsumerD_Eps_df$EPS_fcast95l[1] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 95`[1])*ConsumerD_Eps_df$EPS_ConsumerD_shift[1]

for(i in 2:3){
  ConsumerD_Eps_df$EPS_fcast[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Point Forecast`[i]) *ConsumerD_Eps_df$EPS_fcast[i-1]
  ConsumerD_Eps_df$EPS_fcast80h[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 80`[i]) *ConsumerD_Eps_df$EPS_fcast80h[i-1]
  ConsumerD_Eps_df$EPS_fcast80l[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 80`[i]) *ConsumerD_Eps_df$EPS_fcast80l[i-1]
  ConsumerD_Eps_df$EPS_fcast95h[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Hi 95`[i])*ConsumerD_Eps_df$EPS_fcast95h[i-1]
  ConsumerD_Eps_df$EPS_fcast95l[i] <- (1+ConsumerD_Eps_df$`forecast_CD.Lo 95`[i])*ConsumerD_Eps_df$EPS_fcast95l[i-1]
}

#Valuating the model without macro data
ConsumerD_Eps_df <- cbind(ConsumerD_Eps_df,ConsumerD$DATE[20:22])
CD_macro_N <- accuracy(ConsumerD_Eps_df$EPS_fcast, ConsumerD_Eps_df$EPS_ConsumerD)
#Better than previous

#Combine with the actual data
ConsumerD_Eps_df$`ConsumerD$DATE[20:22]` <- as.character(ConsumerD_Eps_df$`ConsumerD$DATE[20:22]` )
CD_3$DATE <- as.character(c("2020-10-01", "2021-10-01", "2022-10-01"))
ConsumerD_Eps_df <- merge(ConsumerD_Eps_df,CD_3, by.x =  "ConsumerD$DATE[20:22]", by.y = "DATE")
#Analyst's forecast
CD_Macro_fcast <-accuracy(as.numeric(ConsumerD_Eps_df$EPS),ConsumerD_Eps_df$EPS_ConsumerD)
#Still worse than analyst

#Combine the data for plot
CD_fcast_N <- data.frame(c("Date","EPS","Data_Type"))
CD_fcast_N$Date <-  as.character(ConsumerD_Eps_df$`ConsumerD$DATE[20:22]`)
CD_fcast_N$EPS <- ConsumerD_Eps_df$EPS_fcast
CD_fcast_N$Data_Type <- "Forecast Without Macro"


##Plot
CD <- rbind(CD_fcast,CD_fcast_N, CD_analyst,CD_actual)
CD$Date <- as.Date(CD$Date)
CD$EPS <- as.numeric(CD$EPS)
CD <- CD[,-1]

ggplot(CD, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#Analyst's model > tslm model without macro data > tslm model with macro data


#Modeling for Healthcare Sector
#Linear model 1
model_Healthcare <- lm(EPS_Change  ~ ., data = my_data_subsetHC)
summary(model_Healthcare)
CV(model_Healthcare)
#Model with too many not significant variable

#Linear Model 2 after removing with variable with higher p-value
model_Healthcare2 <- lm(EPS_Change  ~ Sales_Revenue_Growth + COGS_Growth + EBIT_Growth +
                         Pretax_Income_Growth + Free_Cash_Flow_Margin + FEDFUNDS , data = my_data_subsetHC)
summary(model_Healthcare2)
CV(model_Healthcare2)
#Better

#Timeseries model with Macro data
model_HC_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth + COGS_Growth + EBIT_Growth + Pretax_Income_Growth
                    + Dividend_Growth + Gross_Margin + EBIT_Margin + Pretax_Margin
                    + Net_Margin + Free_Cash_Flow_Margin + Price_Book_Value + Price_Cash_Flow 
                    + Return_on_Equity + Dividend_Yield + Book_Value_Per_Share+inflation_rate + Recession,
                    data = Healthcare_Eps_ts1)

summary(model_HC_ts)
CV(model_HC_ts)

#Get the actual data set for valuating the model
varHC <- append(varHC,"Recession")
x_HC <- Healthcare_Eps_ts2[1:3,varHC]
x_HC <- as.data.frame(x_HC)

#Using the model for prediction
forecast_HC <- forecast(model_HC_ts,newdata = x_HC, h = 1)

#plot the prediction
plot(forecast_HC)

#Combine the dataset
Healthcare_Eps_ts2 <- cbind(Healthcare_Eps_ts2,forecast_HC,EPS_HC,EPS_HC_shift)
HC_Eps_df <- as.data.frame(Healthcare_Eps_ts2)

#Compute the real EPS using EPS Change
HC_Eps_df$EPS_fcast[1] <- (1+HC_Eps_df$`forecast_HC.Point Forecast`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast80h[1] <- (1+HC_Eps_df$`forecast_HC.Hi 80`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast80l[1] <- (1+HC_Eps_df$`forecast_HC.Lo 80`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast95h[1] <- (1+HC_Eps_df$`forecast_HC.Hi 95`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast95l[1] <- (1+HC_Eps_df$`forecast_HC.Lo 95`[1])*HC_Eps_df$EPS_HC_shift[1]

for(i in 2:3){
HC_Eps_df$EPS_fcast[i] <- (1+HC_Eps_df$`forecast_HC.Point Forecast`[i]) *HC_Eps_df$EPS_fcast[i-1]
HC_Eps_df$EPS_fcast80h[i] <- (1+HC_Eps_df$`forecast_HC.Hi 80`[i]) *HC_Eps_df$EPS_fcast80h[i-1]
HC_Eps_df$EPS_fcast80l[i] <- (1+HC_Eps_df$`forecast_HC.Lo 80`[i]) *HC_Eps_df$EPS_fcast80l[i-1]
HC_Eps_df$EPS_fcast95h[i] <- (1+HC_Eps_df$`forecast_HC.Hi 95`[i])*HC_Eps_df$EPS_fcast95h[i-1]
HC_Eps_df$EPS_fcast95l[i] <- (1+HC_Eps_df$`forecast_HC.Lo 95`[i])*HC_Eps_df$EPS_fcast95l[i-1]
}

HC_Eps_df <- cbind(HC_Eps_df,Healthcare$DATE[20:22])

#accuaracy of the model with macro data
HC_macro <- accuracy(HC_Eps_df$EPS_fcast, HC_Eps_df$EPS_HC)
#not bad

#analyst's forecast
HC_Eps_df$`Healthcare$DATE[20:22]` <- as.character(HC_Eps_df$`Healthcare$DATE[20:22]` )
Healthcare_3$DATE <- as.character(c("2020-10-01", "2021-10-01", "2022-10-01"))
HC_Eps_df <- merge(HC_Eps_df,Healthcare_3, by.x = "Healthcare$DATE[20:22]", by.y = "DATE")
HC_Macro_fcast <-accuracy(as.numeric(HC_Eps_df$EPS),HC_Eps_df$EPS_HC)
#Still better than our model

#combine the data set for plotting
HC_fcast <- data.frame(c("Date","EPS","Data_Type"))
HC_fcast$Date <-  HC_Eps_df$`Healthcare$DATE[20:22]`
HC_fcast$EPS <- HC_Eps_df$EPS_fcast
HC_fcast$Data_Type <- "Forecast with Macro"

HC_analyst <- data.frame(c("Date","EPS","Data_Type"))
HC_analyst$Date <-  HC_Eps_df$`Healthcare$DATE[20:22]`
HC_analyst$EPS <- HC_Eps_df$EPS
HC_analyst$Data_Type <- "Analyst Forecast"

HC_actual <- data.frame(c("Date","EPS","Data_Type"))
HC_actual$Date <-  HC_Eps_df$`Healthcare$DATE[20:22]`
HC_actual$EPS <- HC_Eps_df$EPS_HC
HC_actual$Data_Type <- "Actual EPS"

#model without macro data
model_HC_ts <- tslm(EPS_Change ~ Sales_Revenue_Growth + COGS_Growth + EBIT_Growth + Pretax_Income_Growth
                            + Dividend_Growth + Gross_Margin + EBIT_Margin + Pretax_Margin
                            + Net_Margin + Free_Cash_Flow_Margin + Price_Book_Value + Price_Cash_Flow 
                            + Return_on_Equity + Dividend_Yield + Book_Value_Per_Share, data = Healthcare_Eps_ts1)

summary(model_HC_ts)
CV(model_HC_ts)

#Using model to forecast
forecast_HC <- forecast(model_HC_ts,newdata = x_HC, h = 1)
plot(forecast_HC)

#Combine the data
Healthcare_Eps_ts2 <- cbind(Healthcare_Eps_ts2,forecast_HC,EPS_HC,EPS_HC_shift)
HC_Eps_df <- as.data.frame(Healthcare_Eps_ts2)

#Calculate the real EPS by using EPS_Chang
HC_Eps_df$EPS_fcast[1] <- (1+HC_Eps_df$`forecast_HC.Point Forecast`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast80h[1] <- (1+HC_Eps_df$`forecast_HC.Hi 80`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast80l[1] <- (1+HC_Eps_df$`forecast_HC.Lo 80`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast95h[1] <- (1+HC_Eps_df$`forecast_HC.Hi 95`[1])*HC_Eps_df$EPS_HC_shift[1]
HC_Eps_df$EPS_fcast95l[1] <- (1+HC_Eps_df$`forecast_HC.Lo 95`[1])*HC_Eps_df$EPS_HC_shift[1]

for(i in 2:3){
  HC_Eps_df$EPS_fcast[i] <- (1+HC_Eps_df$`forecast_HC.Point Forecast`[i]) *HC_Eps_df$EPS_fcast[i-1]
  HC_Eps_df$EPS_fcast80h[i] <- (1+HC_Eps_df$`forecast_HC.Hi 80`[i]) *HC_Eps_df$EPS_fcast80h[i-1]
  HC_Eps_df$EPS_fcast80l[i] <- (1+HC_Eps_df$`forecast_HC.Lo 80`[i]) *HC_Eps_df$EPS_fcast80l[i-1]
  HC_Eps_df$EPS_fcast95h[i] <- (1+HC_Eps_df$`forecast_HC.Hi 95`[i])*HC_Eps_df$EPS_fcast95h[i-1]
  HC_Eps_df$EPS_fcast95l[i] <- (1+HC_Eps_df$`forecast_HC.Lo 95`[i])*HC_Eps_df$EPS_fcast95l[i-1]
}

#accuracy of the model without macro data
HC_Eps_df <- cbind(HC_Eps_df,Healthcare$DATE[20:22])
HC_macro_N <- accuracy(HC_Eps_df$EPS_fcast, HC_Eps_df$EPS_HC)
#Better than before and better than analyst
HC_Eps_df$Error <- abs(HC_Eps_df$EPS_fcast - HC_Eps_df$EPS_HC)/HC_Eps_df$EPS_HC

#Analyst's forecast 
HC_Eps_df$`Healthcare$DATE[20:22]` <- as.character(HC_Eps_df$`Healthcare$DATE[20:22]` )
Healthcare_3$DATE <- as.character(c("2020-10-01", "2021-10-01", "2022-10-01"))
HC_Eps_df <- merge(HC_Eps_df,Healthcare_3, by.x = "Healthcare$DATE[20:22]", by.y = "DATE")
HC_Macro_fcast <-accuracy(as.numeric(HC_Eps_df$EPS),HC_Eps_df$EPS_Healthcare)
#We are better than analyst now!

#Combine the data for ploting
HC_fcast_N <- data.frame(c("Date","EPS","Data_Type"))
HC_fcast_N$Date <-  as.character(HC_Eps_df$`Healthcare$DATE[20:22]`)
HC_fcast_N$EPS <- HC_Eps_df$EPS_fcast
HC_fcast_N$Data_Type <- "Forecast Without Macro"


##Plot
HC <- rbind(HC_fcast, HC_fcast_N, HC_analyst,HC_actual)
HC$Date <- as.Date(HC$Date)
HC$EPS <- as.numeric(HC$EPS)
HC <- HC[,-1]

ggplot(HC, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
#For Healthcare Sector, LSTM Model without macro > analyst's forecast > LSTM model with macro


########################################################################################
### Trying to find a better model
### Dynamic Linear Model using quarterly data
#Loading the forecast factset data
load("/Users/zhangwy/Desktop/Capstone Project/factset_forecasts.rda")
combined_forecasts <- combined_forecasts[,-1]

#Loading the factset data
load("/Users/zhangwy/Desktop/Capstone Project/factset_actuals.rda")

#Converting percentage terms to actual terms
combined_actuals[,1:16] <- combined_actuals[,1:16]/100

#Loading the Macro Index data and merge it
Housing_index <- read.csv("/Users/zhangwy/Desktop/Capstone Project/monthlyPERMIT.csv")
Housing_index$DATE <- as.Date(Housing_index$DATE)
combined_actuals$DATE <- as.Date(combined_actuals$DATE)
combined_actuals <- merge(combined_actuals, Housing_index, by.x = "DATE",by.y = "DATE")
pmi_index <- read.csv("/Users/zhangwy/Desktop/Capstone Project/PMI_NMI DATA.csv")
pmi_index$Date <- as.Date(pmi_index$Date)
combined_actuals <- merge(combined_actuals, pmi_index, by.x = "DATE",by.y = "Date")
combined_actuals$`inflation rate` <- na.spline(combined_actuals$`inflation rate`, along = combined_actuals$DATE, na.rm = FALSE)
combined_actuals$Recession <- ifelse(year(combined_actuals$DATE) == 2001 | year(combined_actuals$DATE) == 2008 | year(combined_actuals$DATE) == 2009 | year(combined_actuals$DATE) == 2020 | year(combined_actuals$DATE) == 2017,1,0)

#Get the quarterly Healthcare data and also M2 macro data
Healthcare <- combined_actuals[combined_actuals$Sector=="Healthcare",]
Healthcare$Year <- year(Healthcare$DATE)
getSymbols("M2SL", src = "FRED") # M2 Money Stock
yearmon <- year(time(M2SL))
annual_m2 <- aggregate(M2SL, as.numeric(yearmon),mean)
Year <- rownames(as.data.frame(annual_m2))
annual_m2 <- cbind(annual_m2,Year)
Healthcare <- merge(Healthcare,annual_m2,by = "Year")
Healthcare$annual_m2 <- as.numeric(Healthcare$annual_m2)
Healthcare <- Healthcare[,-1]
Healthcare <- Healthcare[,-(40:50)]

#Change the data to data table
Healthcare <- data.table(Healthcare)

#Name the column of the data
colnames(Healthcare) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                          "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                          "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                          "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                          "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                          "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                          "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                          "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                          "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                          "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                          "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","realfed","PERMIT","NMI","PMI","Recession","m2ab")

#dynamic linear model with macro data for Healthcare Sector
#library the required package and change the data 
library(dynlm)
mat <- as.xts(Healthcare)
Healthcare_Eps_ts1 <- ts(mat, start = c(2001, 10),frequency = 4)

#Choosing the variables using for modelling
selected_cols <- c("EPS_Change", "COGS_Growth","EBIT_Growth","Return_on_Equity","Price_Earnings",
                   "Pretax_Income_Growth","Dividend_Growth","Gross_Margin", 
                   "FEDFUNDS","PERMIT","NMI","PMI")
Healthcare_ts <- ts(Healthcare_Eps_ts1[, selected_cols], start = c(2001, 4),end=c(2019,4),frequency = 4)
Healthcare_df <- data.frame(Date = as.Date(time(Healthcare_ts), origin = "1970-01-01"), Healthcare_ts)

#Do the modelling
fit <- dynlm(EPS_Change ~ ., data = Healthcare_df, k = 2)
summary(fit)
CV(fit)
#seems a good model

Healthcare_ts_test <- window(ts(Healthcare_Eps_ts1[, selected_cols], start = c(2001, 4),frequency = 4), start = c(2020, 1))
Healthcare_ts_test <- data.frame(Date = as.Date(time(Healthcare_ts_test), origin = "1970-01-01"), Healthcare_ts_test)
#using the model for prediction
pred <- predict(fit, newdata = Healthcare_ts_test)

#corplot to avoid multcollinearity
corrplot(cor(as.matrix(Healthcare_Eps_ts1[, selected_cols])), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75) 

# Convert Healthcare_ts_test to data frame for plotting
Healthcare_df_test <- data.frame(Date = Healthcare_ts_test$Date, EPS_Change = coredata(Healthcare_ts_test[,2]))

# Add a column to indicate the type of data (test or forecast)
Healthcare_df_test$Data_Type <- "Real Data"
Healthcare_df_pred <- data.frame(Date = Healthcare_ts_test$Date, EPS_Change = pred )
Healthcare_df_pred$Data_Type <- "dynlm with macro"

#accuracy
HC_macro <- accuracy(Healthcare_df_pred$EPS_Change, Healthcare_df_test$EPS_Change)
HC_macro
#0.119 mean absolute error

# model without macro
selected_cols <- c("EPS_Change", "COGS_Growth","EBIT_Growth","Return_on_Equity",
                   "Pretax_Income_Growth","Dividend_Growth","Gross_Margin", "Price_Earnings")
Healthcare_ts <- ts(Healthcare_Eps_ts1[, selected_cols], start = c(2001, 4),end=c(2019,4),frequency = 4)
Healthcare_df <- data.frame(Date = as.Date(time(Healthcare_ts), origin = "1970-01-01"), Healthcare_ts)

#Model
fit <- dynlm(EPS_Change ~ ., data = Healthcare_df, k = 2)
Healthcare_ts_test <- window(ts(Healthcare_Eps_ts1[, selected_cols], start = c(2001, 4),frequency = 4), start = c(2020, 1))
Healthcare_ts_test <- data.frame(Date = as.Date(time(Healthcare_ts_test), origin = "1970-01-01"), Healthcare_ts_test)
pred2 <- predict(fit, newdata = Healthcare_ts_test)


# Add a column to indicate the type of data (test or forecast)
Healthcare_df_pred2 <- data.frame(Date = Healthcare_ts_test$Date, EPS_Change = pred2)
Healthcare_df_pred2$Data_Type <- "dynlm without macro"

#accuracy
HC_macro_N <- accuracy(Healthcare_df_pred2$EPS_Change, Healthcare_df_test$EPS_Change)
HC_macro_N
#0.130 mean absolute error less than the model with macro data

#tslm model
#Separate the train data and test data
mat2 <- as.xts(Healthcare[1:73,])
mat3 <- as.xts(Healthcare[74:86,])
Healthcare_Eps_ts1 <- ts(mat2, start = c(2001, 1),end = c(2019, 4),frequency = 4)
Healthcare_Eps_ts2 <- ts(mat3, start = c(2020, 1),end = c(2023, 1),frequency = 4)

#The same model before just using quarterly data
model_HC_ts <- tslm(EPS_Change ~  COGS_Growth + Pretax_Income_Growth
                    + Dividend_Growth + EBIT_Margin + Pretax_Margin
                    + Net_Margin + Free_Cash_Flow_Margin + Price_Book_Value + Price_Cash_Flow 
                    + Return_on_Equity + Dividend_Yield + Book_Value_Per_Share + Recession,
                    data = Healthcare_Eps_ts1)
summary(model_HC_ts)
CV(model_HC_ts)

#Get the actual data
x_HC <- Healthcare_Eps_ts2[1:13,]
x_HC <- as.data.frame(x_HC)

#Make the prediction
forecast_HC <- predict(model_HC_ts, newdata = x_HC, h = 1)

#accuracy
accuracy(forecast_HC, mat3$EPS_Change)
#0.177 mean average absolute error, worse than new dynamic linear regression model

# Add a column to indicate the type of data (test or forecast)
Healthcare_df_pred3 <- data.frame(Date = Healthcare_ts_test$Date, EPS_Change = forecast_HC)
Healthcare_df_pred3$Data_Type <- "tslm model"
Healthcare_df_combined <- rbind(Healthcare_df_test, Healthcare_df_pred, Healthcare_df_pred2, Healthcare_df_pred3)

# Convert Date column to class Date
Healthcare_df_combined$Date <- as.Date(Healthcare_df_combined$Date)

# Plot the combined data using ggplot2
ggplot(Healthcare_df_combined, aes(x = Date, y = EPS_Change, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Time Series Forecast",
       y = "EPS_Change",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
#In the graph, we can easily see that dynlm model with macro data > dynlm model without macro data > tslm model


# Get the Consumer Discretionary annually data and add m2 macro data
ConsumerD <- combined_actuals[combined_actuals$Sector=="Consumer Discretionary",]
ConsumerD$Year <- year(ConsumerD$DATE)
getSymbols("M2SL", src = "FRED") # M2 Money Stock
yearmon <- year(time(M2SL))
annual_m2 <- aggregate(M2SL, as.numeric(yearmon),mean)
Year <- rownames(as.data.frame(annual_m2))
annual_m2 <- cbind(annual_m2,Year)
ConsumerD <- merge(ConsumerD,annual_m2,by = "Year")
ConsumerD$annual_m2 <- as.numeric(ConsumerD$annual_m2)
ConsumerD <- ConsumerD[,-1]
ConsumerD <- ConsumerD[,-(40:50)]

#Change the data to data table
ConsumerD <- data.table(ConsumerD)

#Rename the column name
colnames(ConsumerD) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
                         "Net_Income_Growth", "Dividend_Growth", "EPS_Change", "Dividends_Change", 
                         "Gross_Margin", "EBIT_Margin", "EBITDA_Margin", "Pretax_Margin", 
                         "Net_Margin", "Free_Cash_Flow_Margin", "Price_Earnings", 
                         "Price_Earnings_excl", "Price_Sales", "Price_Book_Value", 
                         "Price_Cash_Flow", "Price_Cash_Flow_excl", 
                         "Price_Free_Cash_Flow", "Enterprise_Value_EBIT", 
                         "Enterprise_Value_EBITDA", "Enterprise_Value_Sales", 
                         "Return_on_Equity", "Dividend_Yield", "EPS_recurrent_earnings",
                         "Sales_Per_Share", "Dividends_Per_Share", "Book_Value_Per_Share", 
                         "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","Sector","FEDFUNDS","inflation_rate","realfed","PERMIT","NMI","PMI","Recession","m2ab")

#dynamic linear model with macro data for Consumer Discretionary sector
mat <- as.xts(ConsumerD)
ConsumerD_Eps_ts1 <- ts(mat, start = c(2001, 10),frequency = 4)

#Select the variables used for modelling
selected_cols <- c("EPS_Change", "COGS_Growth","EBIT_Growth","Return_on_Equity",
                   "Pretax_Income_Growth", "Price_Earnings",
                   "FEDFUNDS","inflation_rate","NMI","PMI","Recession")
ConsumerD_ts <- ts(ConsumerD_Eps_ts1[, selected_cols], start = c(2001, 4),end=c(2019,4),frequency = 4)
ConsumerD_df <- data.frame(Date = as.Date(time(ConsumerD_ts), origin = "1970-01-01"), ConsumerD_ts)
#dynlm model
fit <- dynlm(EPS_Change ~ ., data = ConsumerD_df, k = 2)
summary(fit)
CV(fit)
ConsumerD_ts_test <- window(ts(ConsumerD_Eps_ts1[, selected_cols], start = c(2001, 4),frequency = 4), start = c(2020, 1))
ConsumerD_ts_test <- data.frame(Date = as.Date(time(ConsumerD_ts_test), origin = "1970-01-01"), ConsumerD_ts_test)

#using model to do the prediction
pred <- predict(fit, newdata = ConsumerD_ts_test)

#corplot to avoid multicollinearity
corrplot(cor(as.matrix(ConsumerD_Eps_ts1[, selected_cols])), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75) 


# Convert ConsumerD_ts_test to data frame for plotting
ConsumerD_df_test <- data.frame(Date = ConsumerD_ts_test$Date, EPS_Change = coredata(ConsumerD_ts_test[,2]))

# Add a column to indicate the type of data (test or forecast)
ConsumerD_df_test$Data_Type <- "Real Data"
ConsumerD_df_pred <- data.frame(Date = ConsumerD_ts_test$Date, EPS_Change = pred )
ConsumerD_df_pred$Data_Type <- "dynlm macro"

#accuracy of the model
CD_macro <- accuracy(pred, ConsumerD_df_test$EPS_Change)
#0.426 mean absolute error


# Dynamic linear model without macro data
selected_cols <- c("EPS_Change", "COGS_Growth","EBIT_Growth","Return_on_Equity",
                   "Pretax_Income_Growth", "Price_Earnings")
ConsumerD_ts <- ts(ConsumerD_Eps_ts1[, selected_cols], start = c(2001, 4),end=c(2019,4),frequency = 4)
ConsumerD_df <- data.frame(Date = as.Date(time(ConsumerD_ts), origin = "1970-01-01"), ConsumerD_ts)
fit <- dynlm(EPS_Change ~ ., data = ConsumerD_df, k = 2)
summary(fit)
CV(fit)
ConsumerD_ts_test <- window(ts(ConsumerD_Eps_ts1[, selected_cols], start = c(2001, 4),frequency = 4), start = c(2020, 1))
ConsumerD_ts_test <- data.frame(Date = as.Date(time(ConsumerD_ts_test), origin = "1970-01-01"), ConsumerD_ts_test)
pred2 <- predict(fit, newdata = ConsumerD_ts_test)

#corplot avoid multicollinearity
corrplot(cor(as.matrix(ConsumerD_Eps_ts1[, selected_cols])), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75) 

# Add a column to indicate the type of data (test or forecast)
ConsumerD_df_pred2 <- data.frame(Date = ConsumerD_ts_test$Date, EPS_Change = pred2)
ConsumerD_df_pred2$Data_Type <- "dynlm without macro"
ConsumerD_df_combined <- rbind(ConsumerD_df_test, ConsumerD_df_pred, ConsumerD_df_pred2)

#accuracy
CD_macro_N <- accuracy(pred2, ConsumerD_df_test$EPS_Change)
#0.254 mean absolute error

#tslm model just done before, just change annually to quarterly
mat4 <- as.xts(ConsumerD[1:73,])
mat5 <- as.xts(ConsumerD[74:86,])
ConsumerD_Eps_ts1 <- ts(mat4, start = c(2001, 1),end = c(2019, 4),frequency = 4)
ConsumerD_Eps_ts2 <- ts(mat5, start = c(2020, 1),end = c(2023, 1),frequency = 4)

model_CD_ts <- tslm(EPS_Change ~  COGS_Growth + Pretax_Income_Growth
                    + Dividend_Growth + EBIT_Margin
                    + Net_Margin + Free_Cash_Flow_Margin + Price_Cash_Flow 
                    + Return_on_Equity + Dividend_Yield + Book_Value_Per_Share + Recession,
                    data = ConsumerD_Eps_ts1)
summary(model_CD_ts)
CV(model_CD_ts)
x_CD <- ConsumerD_Eps_ts2[1:13,]
x_CD <- as.data.frame(x_CD)
pred3 <- predict(model_CD_ts, newdata = x_CD, h = 1)
accuracy(pred3, mat5$EPS_Change)
#1.034 mean absolute error, worse than new dynlm model

# Add a column to indicate the type of data (test or forecast)
ConsumerD_df_pred3 <- data.frame(Date = ConsumerD_ts_test$Date, EPS_Change = pred3)
ConsumerD_df_pred3$Data_Type <- "tslm Forecast"
ConsumerD_df_combined <- rbind(ConsumerD_df_test, ConsumerD_df_pred, ConsumerD_df_pred2, ConsumerD_df_pred3)

# Convert Date column to class Date
ConsumerD_df_combined$Date <- as.Date(ConsumerD_df_combined$Date)

# Plot the combined data using ggplot2
ggplot(ConsumerD_df_combined, aes(x = Date, y = EPS_Change, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "6 months") +
  labs(title = "Time Series Forecast",
       y = "EPS_Change",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#In the graph, we can see that dynlm model without macro > dynlm model with macro > tslm model
#We may not find the suitable macro data for consumer discretionary sector





