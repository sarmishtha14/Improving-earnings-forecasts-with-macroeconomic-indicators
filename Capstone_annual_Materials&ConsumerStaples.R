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
#Materials
Materials_analyst <- combined_forecasts[combined_forecasts$Sector == "Materials",]
Materials_3 <- Materials_analyst[12:14,]
#Consumer Staples
CS_analyst <- combined_forecasts[combined_forecasts$Sector == "Consumer Staples",]
CS_3 <- CS_analyst[12:14,]

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
plotmeans(combined_actuals$`EPS Change` ~ year(combined_actuals$DATE), main="EPS Growth across Years", data=combined_actuals, xlab = "Year", ylab = "EPS Growth")

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

#Materials Sector EDA

#Materials Industry
Materials <- combined_actuals[combined_actuals$Sector=="Materials",]
Materials$Year <- year(Materials$DATE)
getSymbols("M2SL", src = "FRED") # M2 Money Stock
yearmon <- year(time(M2SL))
annual_m2 <- aggregate(M2SL, as.numeric(yearmon),mean)
Year <- rownames(as.data.frame(annual_m2))
annual_m2 <- cbind(annual_m2,Year)
Materials <- merge(Materials,annual_m2,by = "Year")
Materials<-Materials[,-(40:51)]
Materials$DATE <- as.Date(Materials$DATE)
Materials <- Materials[month(Materials$DATE) == 10,]
Materials$annual_m2 <- as.numeric(Materials$annual_m2)


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
                                 "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT","Recession","annual_m2")]
colnames(Materials) <- c("Year","DATE","Sales_Revenue_Growth","COGS_Growth","Gross_Income_Growth","EBIT_Growth", "EBITDA_Growth", "Pretax_Income_Growth", 
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
                               "Cash_Flow_Per_Share", "Free_Cash_Flow_Per_Share","FEDFUNDS","inflation_rate","PERMIT","Recession","annual_m2")

EPS_Materials <- Materials[20:22,"EPS_recurrent_earnings"]
EPS_Materials_shift <- Materials[19:21,"EPS_recurrent_earnings"]


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
m1 <- merge(my_data_subsetM,Materials[,c(2,11,44)], by = "EPS_Change")
m1 <- m1[order(DATE)]
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
Materials_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
Materials_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2024),frequency = 1)

#Seasonality
autoplot(Materials_Eps_ts1)

#Modeling for Materials Sector
#Linear model
model_materials <- lm(EPS_Change~ ., data = my_data_subsetM)
summary(model_materials)
CV(model_materials)

#Timeseries model with macro
model_materials_ts <- tslm(EPS_Change ~ Pretax_Income_Growth+      
                           Dividends_Change + Gross_Margin + Price_Free_Cash_Flow+
                           Dividends_Per_Share  + FEDFUNDS+PERMIT + Recession+annual_m2,data = Materials_Eps_ts1)

#In sample accuracy of model with macros
summary(model_materials_ts)
CV(model_materials_ts)
x_M <- Materials_Eps_ts2
x_M <- as.data.frame(x_M)

Materials_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
forecast_M <- forecast(model_materials_ts,newdata = x_M, h = 1)
plot(forecast_M)
Materials_Eps_ts2 <- cbind(Materials_Eps_ts2,forecast_M,EPS_Materials,EPS_Materials_shift)
Materials_Eps_df <- as.data.frame(Materials_Eps_ts2)

Materials_Eps_df$EPS_fcast[1] <- (1+Materials_Eps_df$`forecast_M.Point Forecast`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast80h[1] <- (1+Materials_Eps_df$`forecast_M.Hi 80`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast80l[1] <- (1+Materials_Eps_df$`forecast_M.Lo 80`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast95h[1] <- (1+Materials_Eps_df$`forecast_M.Hi 95`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast95l[1] <- (1+Materials_Eps_df$`forecast_M.Lo 95`[1])*Materials_Eps_df$EPS_Materials_shift[1]

for(i in 2:3){
  Materials_Eps_df$EPS_fcast[i] <- (1+Materials_Eps_df$`forecast_M.Point Forecast`[i]) *Materials_Eps_df$EPS_fcast[i-1]
  Materials_Eps_df$EPS_fcast80h[i] <- (1+Materials_Eps_df$`forecast_M.Hi 80`[i]) *Materials_Eps_df$EPS_fcast80h[i-1]
  Materials_Eps_df$EPS_fcast80l[i] <- (1+Materials_Eps_df$`forecast_M.Lo 80`[i]) *Materials_Eps_df$EPS_fcast80l[i-1]
  Materials_Eps_df$EPS_fcast95h[i] <- (1+Materials_Eps_df$`forecast_M.Hi 95`[i])*Materials_Eps_df$EPS_fcast95h[i-1]
  Materials_Eps_df$EPS_fcast95l[i] <- (1+Materials_Eps_df$`forecast_M.Lo 95`[i])*Materials_Eps_df$EPS_fcast95l[i-1]
}

#Finding the out of sample accuracy
Materials_Eps_df <- cbind(Materials_Eps_df,Materials$DATE[20:22])
Mat_Macro <-accuracy(Materials_Eps_df$EPS_fcast,Materials_Eps_df$EPS_Materials)

#Finding accuracy of analyst forecast
Materials_Eps_df$`Materials$DATE[20:22]` <- as.character(Materials_Eps_df$`Materials$DATE[20:22]` )
Materials_Eps_df <- merge(Materials_Eps_df,Materials_analyst, by.x = "Materials$DATE[20:22]", by.y = "DATE")
Mat_Macro_fcast <-accuracy(as.numeric(Materials_Eps_df$EPS),Materials_Eps_df$EPS_Materials)

#adding to a db
Ma_fcast <- data.frame(c("Date","EPS","Data_Type"))
Ma_fcast$Date <-  Materials_Eps_df$`Materials$DATE[20:22]`
Ma_fcast$EPS <- Materials_Eps_df$EPS_fcast
Ma_fcast$Data_Type <- "Forecast with Macro"

Ma_anal <- data.frame(c("Date","EPS","Data_Type"))
Ma_anal$Date <-  Materials_Eps_df$`Materials$DATE[20:22]`
Ma_anal$EPS <- Materials_Eps_df$EPS
Ma_anal$Data_Type <- "Analyst Forecast"

Ma_actual <- data.frame(c("Date","EPS","Data_Type"))
Ma_actual$Date <-  Materials_Eps_df$`Materials$DATE[20:22]`
Ma_actual$EPS <- Materials_Eps_df$EPS_Materials
Ma_actual$Data_Type <- "Actual EPS"


#Timeseries model without macro
Materials_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
model_materials_ts <- tslm(EPS_Change ~ Pretax_Income_Growth+      
                             Dividends_Change + Gross_Margin + Price_Free_Cash_Flow+
                             Dividends_Per_Share  + 
                             Recession,data = Materials_Eps_ts1)

#In sample accuracy of model without macros
summary(model_materials_ts)
CV(model_materials_ts)
Materials_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)

varM <- append(varM,"annual_m2")
x_M <- Materials_Eps_ts2
x_M <- as.data.frame(x_M)

#Finding the Forecasted EPS from EPS Growth
Materials_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
forecast_M <- forecast(model_materials_ts,newdata = x_M, h = 1)
plot(forecast_M)
Materials_Eps_ts2 <- cbind(Materials_Eps_ts2,forecast_M,EPS_Materials,EPS_Materials_shift)
Materials_Eps_df <- as.data.frame(Materials_Eps_ts2)

Materials_Eps_df$EPS_fcast[1] <- (1+Materials_Eps_df$`forecast_M.Point Forecast`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast80h[1] <- (1+Materials_Eps_df$`forecast_M.Hi 80`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast80l[1] <- (1+Materials_Eps_df$`forecast_M.Lo 80`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast95h[1] <- (1+Materials_Eps_df$`forecast_M.Hi 95`[1])*Materials_Eps_df$EPS_Materials_shift[1]
Materials_Eps_df$EPS_fcast95l[1] <- (1+Materials_Eps_df$`forecast_M.Lo 95`[1])*Materials_Eps_df$EPS_Materials_shift[1]

for(i in 2:3){
  Materials_Eps_df$EPS_fcast[i] <- (1+Materials_Eps_df$`forecast_M.Point Forecast`[i]) *Materials_Eps_df$EPS_fcast[i-1]
  Materials_Eps_df$EPS_fcast80h[i] <- (1+Materials_Eps_df$`forecast_M.Hi 80`[i]) *Materials_Eps_df$EPS_fcast80h[i-1]
  Materials_Eps_df$EPS_fcast80l[i] <- (1+Materials_Eps_df$`forecast_M.Lo 80`[i]) *Materials_Eps_df$EPS_fcast80l[i-1]
  Materials_Eps_df$EPS_fcast95h[i] <- (1+Materials_Eps_df$`forecast_M.Hi 95`[i])*Materials_Eps_df$EPS_fcast95h[i-1]
  Materials_Eps_df$EPS_fcast95l[i] <- (1+Materials_Eps_df$`forecast_M.Lo 95`[i])*Materials_Eps_df$EPS_fcast95l[i-1]
}

#Out of sample accuracy
Materials_Eps_df <- cbind(Materials_Eps_df,Materials$DATE[20:22])
Mat_Macro_N <-accuracy(Materials_Eps_df$EPS_fcast,Materials_Eps_df$EPS_Materials)

#DF
Ma_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
Ma_fcast_M$Date <- as.character(Materials_Eps_df$`Materials$DATE[20:22]`)
Ma_fcast_M$EPS <- Materials_Eps_df$EPS_fcast
Ma_fcast_M$Data_Type <- "Forecast Without Macro"

#Plot
dev.off()
Ma <- rbind(Ma_fcast,Ma_fcast_M,Ma_anal,Ma_actual)
Ma$Date <- as.Date(Ma$Date)
Ma$EPS <- as.numeric(Ma$EPS)
Ma <- Ma[,-1]

ggplot(Ma, aes(x = Date, y = EPS, color = Data_Type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 year") +
  labs(title = "Time Series Forecast",
       y = "EPS",
       color = "Data Type") +
  theme_bw() +
  theme(plot.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#####################################################################################################################################
#Consumer Staples EDA

#ConsumerStaples ts
ConsumerStaples <- combined_actuals[combined_actuals$Sector=="Consumer Staples",]
ConsumerStaples<- ConsumerStaples[,-(39:50)]
ConsumerStaples <- ConsumerStaples[month(ConsumerStaples$DATE) == 10,]

#Exploratory data analysis
ggplot(data = ConsumerStaples, mapping = aes(x=ConsumerStaples$DATE )) +
  geom_line(mapping = aes(y = ConsumerStaples$`COGS Growth`), color = "blue") +
  geom_line(aes(y = ConsumerStaples$`inflation rate`), color = "red") +
  geom_line(aes(y = ConsumerStaples$`EBIT Growth`), color = "green") +
  geom_line(aes(y = ConsumerStaples$`FEDFUNDS`), color = "black") +
  geom_line(aes(y = ConsumerStaples$`EPS Change`), color = "grey") 

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
                                        "Cash Flow Per Share", "Free Cash Flow Per Share","FEDFUNDS","inflation rate","PERMIT","Recession")]

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

EPS_CS <- ConsumerStaples[20:22,"EPS_recurrent_earnings"]
EPS_CS_shift <- ConsumerStaples[19:21,"EPS_recurrent_earnings"]

# Calculate the correlation matrix for the remaining variables
cor_matrix_remainingCS <- cor(as.matrix(my_data_subsetCS))

# Find the variables that are highly correlated with each other, excluding the diagonal
highly_correlated_varsCS <- findCorrelation(cor_matrix_remainingCS, cutoff = 0.7)
highly_correlated_varsCS <- highly_correlated_varsCS[highly_correlated_varsCS!=9]

# Remove the highly correlated variables from the data table
my_data_subsetCS <- my_data_subsetCS[, -highly_correlated_varsCS, with = FALSE]
varCS <- colnames(my_data_subsetCS)

# Generate a heatmap of the correlation matrix for the remaining variables
corrplot(cor(as.matrix(my_data_subsetCS)), type = "full", method = "circle", order = "hclust", tl.col = "black", number.cex = 0.75)

#Adding the Recession dummy variable
m1 <- merge(my_data_subsetCS,ConsumerStaples[,c(1,10)], by = "EPS_Change")
m1 <- m1 %>%relocate(DATE, .before = "EPS_Change")
m1 <- m1[order(DATE)]
mat <- as.xts(m1[1:19,])
mat2 <- as.xts(m1[20:22,])
ConsumerStaples_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
ConsumerStaples_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)


#Modeling for Consumer Staples Sector
#Linear model
model_conStaples <- lm(EPS_Change  ~ ., data = my_data_subsetCS)
summary(model_conStaples)
CV(model_conStaples)

#Timeseries model with macros
model_CS_ts <- tslm(EPS_Change ~ COGS_Growth+EBIT_Growth     
                    +Pretax_Margin+Net_Margin+Price_Earnings+Book_Value_Per_Share
                    +FEDFUNDS+PERMIT + Recession,data = ConsumerStaples_Eps_ts1)

#In sample accuracy of model with macros
summary(model_CS_ts)
CV(model_CS_ts)
x_CS <- ConsumerStaples_Eps_ts2
x_CS <- as.data.frame(x_CS)

#Finding the Forecasted EPS from EPS Growth
ConsumerStaples_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
forecast_CS <- forecast(model_CS_ts,newdata = x_CS, h = 1)
plot(forecast_CS)
ConsumerStaples_Eps_ts2 <- cbind(ConsumerStaples_Eps_ts2,forecast_CS,EPS_CS,EPS_CS_shift)
CS_Eps_df <- as.data.frame(ConsumerStaples_Eps_ts2)

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

#Finding the out of sample accuracy
CS_Eps_df <- cbind(CS_Eps_df,ConsumerStaples$DATE[20:22])
CS_macro <- accuracy(CS_Eps_df$EPS_fcast,CS_Eps_df$EPS_CS)

#Finding accuracy of analyst forecast
CS_Eps_df$`ConsumerStaples$DATE[20:22]` <- as.character(CS_Eps_df$`ConsumerStaples$DATE[20:22]` )
CS_Eps_df <- merge(CS_Eps_df,CS_analyst, by.x = "ConsumerStaples$DATE[20:22]", by.y = "DATE")
CS_macro_fcast <- accuracy(as.numeric(CS_Eps_df$EPS),CS_Eps_df$EPS_CS)

#adding to a db
CS_fcast <- data.frame(c("Date","EPS","Data_Type"))
CS_fcast$Date <-  CS_Eps_df$`ConsumerStaples$DATE[20:22]`
CS_fcast$EPS <- CS_Eps_df$EPS_fcast
CS_fcast$Data_Type <- "Forecast with Macro"

CS_anal <- data.frame(c("Date","EPS","Data_Type"))
CS_anal$Date <-  CS_Eps_df$`ConsumerStaples$DATE[20:22]`
CS_anal$EPS <- CS_Eps_df$EPS
CS_anal$Data_Type <- "Analyst Forecast"

CS_actual <- data.frame(c("Date","EPS","Data_Type"))
CS_actual$Date <-  CS_Eps_df$`ConsumerStaples$DATE[20:22]`
CS_actual$EPS <- CS_Eps_df$EPS_CS
CS_actual$Data_Type <- "Actual EPS"

#Timeseries model without macros
ConsumerStaples_Eps_ts1 <- ts(mat, start = c(2001),end = c(2019),frequency = 1)
model_CS_ts <- tslm(EPS_Change ~ COGS_Growth+EBIT_Growth     
                    +Pretax_Margin+Net_Margin+Price_Earnings+Book_Value_Per_Share
                    +Recession,data = ConsumerStaples_Eps_ts1)

#In sample accuract of model without macros
summary(model_CS_ts)
CV(model_CS_ts)
ConsumerStaples_Eps_ts2 <- ts(mat2, start = c(2020),end = c(2022),frequency = 1)
x_CS <- ConsumerStaples_Eps_ts2
x_CS <- as.data.frame(x_CS)

#Finding the Forecasted EPS from EPS Growth
forecast_CS <- forecast(model_CS_ts,newdata = x_CS, h = 1)
plot(forecast_CS)
ConsumerStaples_Eps_ts2 <- cbind(ConsumerStaples_Eps_ts2,forecast_CS,EPS_CS,EPS_CS_shift)
CS_Eps_df <- as.data.frame(ConsumerStaples_Eps_ts2)

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
CS_Eps_df <- cbind(CS_Eps_df,ConsumerStaples$DATE[20:22])
CS_macro_N <- accuracy(CS_Eps_df$EPS_fcast,CS_Eps_df$EPS_CS)

#DF
CS_fcast_M <- data.frame(c("Date","EPS","Data_Type"))
CS_fcast_M$Date <- as.character(CS_Eps_df$`ConsumerStaples$DATE[20:22]`)
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


##########################################################################################
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
