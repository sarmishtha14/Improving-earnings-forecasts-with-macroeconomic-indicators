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

#Loading the factset data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/factset_actuals.rda")

#Converting percentage terms to actual terms
combined_actuals[,1:16] <- combined_actuals[,1:16]/100

#Loading the Housing Index data and merge it
Housing_index <- read.csv("C:/Users/Sarmishtha Jain/Downloads/monthlyPERMIT.csv")
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
data <- combined_actuals[,-36:-35]
colnames(data) <- c("SRG","COGS","GIG","EBIT","EBITDA","PIG","NIG","DG",
                    "EPSG","DC","GM","EBITM","EBITDAM","PM","NM","FCFCM","PE","PEX",
                    "PS","PBV","PCF","PCFX","PVC","EVEBIT","EVEBITDA","EVS",
                    "ROE","DY","EPS","SPS","DPS","BVS","CVS","FCF","FED","INF",
                    "RFED","I","HC","CD","IT","E","F","CS","M","RE","U","TC","R")
M = cor(data[,1:20])
corrplot(M)

#Adding dummy variable for EPS recession
combined_actuals$Recession <- ifelse(year(combined_actuals$DATE) == 2001 | year(combined_actuals$DATE) == 2008 | year(combined_actuals$DATE) == 2009 | year(combined_actuals$DATE) == 2020 | year(combined_actuals$DATE) == 2017,1,0)
#combined_actuals$Recession <- factor(combined_actuals$Recession)

#EPSG and NIG are highly correlated, so we can remove NIG
#DC and DG are highly correlated, so we can remove DC

#Timeseries/Categorical/Continuous/Discrete

#Consumer Staples EDA

#ConsumerStaples ts
ConsumerStaples <- combined_actuals[combined_actuals$Sector=="Consumer Staples",]
ConsumerStaples<-ConsumerStaples[,-(39:50)]

#Exploratory data analysis
ggplot(data = ConsumerStaples, mapping = aes(x=ConsumerStaples$DATE )) +
  geom_line(mapping = aes(y = ConsumerStaples$`COGS Growth`), color = "blue") +
  geom_line(aes(y = ConsumerStaples$`inflation rate`), color = "red") +
  geom_line(aes(y = ConsumerStaples$`EBIT Growth`), color = "green") +
  geom_line(aes(y = ConsumerStaples$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = ConsumerStaples$`EPS Change`), color = "grey") 


#Seasonality
ggseasonplot(ConsumerStaples_Eps_ts )
ggsubseriesplot(ConsumerStaples_Eps_ts)

#Correlations

ConsumerStaples <- data.table(ConsumerStaples)


# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetCS <- ConsumerStaples[, c("Sales/Revenue Growth","COGS Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
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

colnames(ConsumerStaples) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT")

EPS_CS <- ConsumerStaples[77:86,"EPS_recurrent_earnings"]
EPS_CS_shift <- ConsumerStaples[76:85,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingCS <- cor(as.matrix(my_data_subsetCS))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsCS <- findCorrelation(cor_matrix_remainingCS, cutoff = 0.8)

# Remove the highly correlated variables from the data table
my_data_subsetCS <- my_data_subsetCS[, -highly_correlated_varsCS, with = FALSE]
varCS <- colnames(my_data_subsetCS)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetCS)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetCS,ConsumerStaples[,c(1,10,40)], by = "EPS_Change")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
mat <- as.xts(m1[1:76,])
mat2 <- as.xts(m1[77:86,])
ConsumerStaples_Eps_ts1 <- ts(mat, start = c(2001, 10),end = c(2020,3),frequency = 4)
ConsumerStaples_Eps_ts2 <- ts(mat2, start = c(2020, 4),end = c(2023,1),frequency = 4)

#Materials Sector EDA

#Materials Industry
Materials <- combined_actuals[combined_actuals$Sector=="Materials",]
Materials<-Materials[,-(39:50)]
Materials$DATE <- as.Date(Materials$DATE)

#Exploratory data analysis
ggplot(data = Materials, mapping = aes(x=Materials$DATE )) +
  geom_line(mapping = aes(y = Materials$`COGS Growth`), color = "blue") +
  geom_line(aes(y = Materials$`inflation rate`), color = "red") +
  geom_line(aes(y = Materials$`EBIT Growth`), color = "green") +
  geom_line(aes(y = Materials$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = Materials$`EPS Change`), color = "grey") 

#EPS Spikes in the years 2003-2004, 2011-2012, 2018 and 2022
#EPS Steeps in the years 2008-2009 and 2020 (recession)
#EBIT Growth shows similar trend to EPS

#Correlations

Materials <- data.table(Materials)
EPS_Materials <- Materials[77:86,"EPS_recurrent_earnings"]
EPS_Materials_shift <- Materials[76:85,"EPS_recurrent_earnings"]

# Subset the data table to include only the variables you want to use as predictors and the dependent variable
my_data_subsetM <- Materials[, c("Sales/Revenue Growth","COGS Growth","Gross Income Growth","EBIT Growth", "EBITDA Growth", "Pretax Income Growth", 
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
colnames(Materials) <- c("DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT")
# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingM <- cor(as.matrix(my_data_subsetM))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsM <- findCorrelation(cor_matrix_remainingM, cutoff = 0.8)

# Remove the highly correlated variables from the data table
my_data_subsetM <- my_data_subsetM[, -highly_correlated_varsM, with = FALSE]
varM <- colnames(my_data_subsetM)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetM)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetM,Materials[,c(1,10,40)], by = "EPS_Change")
m1 <- m1[order(DATE)]
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
mat <- as.xts(m1[1:76,])
mat2 <- as.xts(m1[77:86,])
Materials_Eps_ts1 <- ts(mat, start = c(2001, 4),end = c(2020,3),frequency = 4)
Materials_Eps_ts2 <- ts(mat2, start = c(2020, 4),end = c(2023,1),frequency = 4)

#Seasonality
autoplot(Materials_Eps_ts1)
ggseasonplot(Materials_Eps_ts1)
ggsubseriesplot(Materials_Eps_ts1)
#gglagplot(Materials_Eps_ts1)
#ggAcf(Materials_Eps_ts1)

#Loading the forecast factset data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/factset_forecasts.rda")
combined_forecasts <- combined_forecasts[,-1]
#Materials
Materials_analyst <- combined_forecasts[combined_forecasts$Sector == "Materials",]
Materials_4 <- Materials_analyst[12:15,]

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

#Modeling for Materials Sector
#Linear model
model_materials <- lm(EPS_Change~ ., data = my_data_subsetM)
summary(model_materials)
CV(model_materials)

#Timeseries model
model_materials_ts <- tslm(EPS_Change ~ COGS_Growth + Gross_Income_Growth + Pretax_Income_Growth + Dividends_Change + Gross_Margin + 
                             Price_Book_Value + Price_Cash_Flow + Price_Free_Cash_Flow + Enterprise_Value_EBITDA + Dividend_Yield +
                             Dividends_Per_Share + FEDFUNDS + inflation_rate + PERMIT,data = Materials_Eps_ts1)

summary(model_materials_ts)
CV(model_materials_ts)
x_M <- Materials_Eps_ts2[1:10,varM]
x_M <- as.data.frame(x_M)

forecast_M <- forecast(model_materials_ts,newdata = x_M, h = 1)
plot(forecast_M)
Materials_Eps_ts2 <- cbind(Materials_Eps_ts2,forecast_M,EPS_Materials,EPS_Materials_shift)
Materials_Eps_df <- as.data.frame(Materials_Eps_ts2)
Materials_Eps_df$EPS_fcast <- (1+Materials_Eps_df$EPS_Materials_shift*Materials_Eps_df$`forecast_M.Point Forecast`)  
Materials_Eps_df$EPS_fcast80h <- (1+Materials_Eps_df$EPS_Materials_shift*Materials_Eps_df$`forecast_M.Hi 80`)  
Materials_Eps_df$EPS_fcast80l <- (1+Materials_Eps_df$EPS_Materials_shift*Materials_Eps_df$`forecast_M.Lo 80`)  
Materials_Eps_df$EPS_fcast95h <- (1+Materials_Eps_df$EPS_Materials_shift*Materials_Eps_df$`forecast_M.Hi 95`)
Materials_Eps_df$EPS_fcast95l <- (1+Materials_Eps_df$EPS_Materials_shift*Materials_Eps_df$`forecast_M.Lo 95`)  
Materials_Eps_df <- cbind(Materials_Eps_df,Materials$DATE[77:86])
Materials_Eps_df$Error <- abs(Materials_Eps_df$EPS_fcast - Materials_Eps_df$EPS_Materials)/Materials_Eps_df$EPS_Materials
avg_error_M <- mean(Materials_Eps_df$Error)

ggplot(data = Materials_Eps_df, mapping = aes(x=Materials_Eps_df$`Materials$DATE[77:86]` )) +
  geom_line(mapping = aes(y = Materials_Eps_df$EPS_Materials), color = "blue") +
  geom_line(aes(y = Materials_Eps_df$EPS_fcast), color = "red")


#Modeling for Consumer Staples Sector
#Linear model
model_conStaples <- lm(EPS_Change  ~ ., data = my_data_subsetCS)
summary(model_conStaples)
CV(model_conStaples)

#Timeseries model
model_CS_ts <- tslm(EPS_Change ~ COGS_Growth + EBITDA_Growth + Pretax_Income_Growth + Dividend_Growth + EBIT_Margin + Pretax_Margin + 
                      + Net_Margin + Free_Cash_Flow_Margin + Price_Cash_Flow + Return_on_Equity + Dividend_Yield +
                      Book_Value_Per_Share + FEDFUNDS + inflation_rate + PERMIT,data = ConsumerStaples_Eps_ts1)

summary(model_CS_ts)
CV(model_CS_ts)
x_CS <- ConsumerStaples_Eps_ts1[1:10,varCS]
x_CS <- as.data.frame(x_CS)
forecast_CS <- forecast(model_CS_ts,newdata = x_CS, h = 1)
plot(forecast_CS)
ConsumerStaples_Eps_ts2 <- cbind(ConsumerStaples_Eps_ts2,forecast_CS,EPS_CS,EPS_CS_shift)
CS_Eps_df <- as.data.frame(ConsumerStaples_Eps_ts2)
CS_Eps_df$EPS_fcast <- (1+CS_Eps_df$EPS_CS_shift*CS_Eps_df$`forecast_CS.Point Forecast`)  
CS_Eps_df$EPS_fcast80h <- (1+CS_Eps_df$EPS_CS_shift*CS_Eps_df$`forecast_CS.Hi 80`)  
CS_Eps_df$EPS_fcast80l <- (1+CS_Eps_df$EPS_CS_shift*CS_Eps_df$`forecast_CS.Lo 80`)  
CS_Eps_df$EPS_fcast95h <- (1+CS_Eps_df$EPS_CS_shift*CS_Eps_df$`forecast_CS.Hi 95`)
CS_Eps_df$EPS_fcast95l <- (1+CS_Eps_df$EPS_CS_shift*CS_Eps_df$`forecast_CS.Lo 95`)  
CS_Eps_df <- cbind(CS_Eps_df,ConsumerStaples$DATE[77:86])
CS_Eps_df$Error <- abs(CS_Eps_df$EPS_fcast - CS_Eps_df$EPS_CS)/CS_Eps_df$EPS_CS
avg_error_CS <- mean(CS_Eps_df$Error)

ggplot(data = CS_Eps_df, mapping = aes(x=CS_Eps_df$`ConsumerStaples$DATE[77:86]` )) +
  geom_line(mapping = aes(y = CS_Eps_df$EPS_CS), color = "blue") +
  geom_line(aes(y = CS_Eps_df$EPS_fcast), color = "red")


