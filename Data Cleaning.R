#install.packages("MASS")
#install.packages("stats")
library('MASS')
library('stats')
library("readxl")
library("dplyr")
library("data.table")

#Data Extraction and Cleaning of Actual Data from Factset

#Real Estate Index Fund (IYR)
IYR_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/IYR_actual.xlsx', range = "A8:CP47")
IYR_actual <- as.data.frame(t(IYR_actual))
colnames(IYR_actual) <- IYR_actual[1,]
IYR_actual <- IYR_actual[-1,]
IYR_actual$date <- rownames(IYR_actual)
colnames(IYR_actual)[11] <- "EPS Change"
colnames(IYR_actual)[12] <- "Dividends Change"
IYR_actual$Sector = "Real Estate"

#Telecommunication ETF (IYZ)
IYZ_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/IYZ_actual.xlsx', range = "A8:CP47")
IYZ_actual <- as.data.frame(t(IYZ_actual))
colnames(IYZ_actual) <- IYZ_actual[1,]
IYZ_actual <- IYZ_actual[-1,]
IYZ_actual$date <- rownames(IYZ_actual)
colnames(IYZ_actual)[11] <- "EPS Change"
colnames(IYZ_actual)[12] <- "Dividends Change"
IYZ_actual$Sector = "Telecommunication"

#Materials ETF (XLB)
XLB_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLB_actual.xlsx', range = "A8:CP47")
XLB_actual <- as.data.frame(t(XLB_actual))
colnames(XLB_actual) <- XLB_actual[1,]
XLB_actual <- XLB_actual[-1,]
XLB_actual$date <- rownames(XLB_actual)
colnames(XLB_actual)[11] <- "EPS Change"
colnames(XLB_actual)[12] <- "Dividends Change"
XLB_actual$Sector = "Materials"

#Consumer Staples ETF (XLP)
XLP_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLP_actual.xlsx', range = "A8:CP47")
XLP_actual <- as.data.frame(t(XLP_actual))
colnames(XLP_actual) <- XLP_actual[1,]
XLP_actual <- XLP_actual[-1,]
XLP_actual$date <- rownames(XLP_actual)
colnames(XLP_actual)[11] <- "EPS Change"
colnames(XLP_actual)[12] <- "Dividends Change"
XLP_actual$Sector = "Consumer Staples"

#Consumer Discretionary ETF (XLY)
XLY_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLY_actual.xlsx', range = "A8:CP47")
XLY_actual <- as.data.frame(t(XLY_actual))
colnames(XLY_actual) <- XLY_actual[1,]
XLY_actual <- XLY_actual[-1,]
XLY_actual$date <- rownames(XLY_actual)
colnames(XLY_actual)[11] <- "EPS Change"
colnames(XLY_actual)[12] <- "Dividends Change"
XLY_actual$Sector = "Consumer Discretionary"

#Healthcare ETF (XLV)
XLV_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLV_actual.xlsx', range = "A8:CP47")
XLV_actual <- as.data.frame(t(XLV_actual))
colnames(XLV_actual) <- XLV_actual[1,]
XLV_actual <- XLV_actual[-1,]
XLV_actual$date <- rownames(XLV_actual)
colnames(XLV_actual)[11] <- "EPS Change"
colnames(XLV_actual)[12] <- "Dividends Change"
XLV_actual$Sector = "Healthcare"

#Technology ETF (XLK)
XLK_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLK_actual.xlsx', range = "A8:CP47")
XLK_actual <- as.data.frame(t(XLK_actual))
colnames(XLK_actual) <- XLK_actual[1,]
XLK_actual <- XLK_actual[-1,]
XLK_actual$date <- rownames(XLK_actual)
colnames(XLK_actual)[11] <- "EPS Change"
colnames(XLK_actual)[12] <- "Dividends Change"
XLK_actual$Sector = "Technology"

#Financials ETF (XLF)
XLF_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLF_actual.xlsx', range = "A8:CP45")
XLF_actual <- as.data.frame(t(XLF_actual))
colnames(XLF_actual) <- XLF_actual[1,]
XLF_actual <- XLF_actual[-1,]
XLF_actual$date <- rownames(XLF_actual)
colnames(XLF_actual)[9] <- "EPS Change"
colnames(XLF_actual)[10] <- "Dividends Change"
COGS_Growth <- as.character(0)
Gross_Income_Growth <- as.character(0)
XLF_actual <- cbind(XLF_actual[,1:2],COGS_Growth,Gross_Income_Growth,XLF_actual[,3:38])
colnames(XLF_actual)[1] <- "Growth - Year over Year %"
colnames(XLF_actual)[3] <- "COGS Growth"
colnames(XLF_actual)[4] <- "Gross Income Growth"
XLF_actual$Sector = "Financials"

#Utilities ETF (XLU)
XLU_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLU_actual.xlsx', range = "A8:CP47")
XLU_actual <- as.data.frame(t(XLU_actual))
colnames(XLU_actual) <- XLU_actual[1,]
XLU_actual <- XLU_actual[-1,]
XLU_actual$date <- rownames(XLU_actual)
colnames(XLU_actual)[11] <- "EPS Change"
colnames(XLU_actual)[12] <- "Dividends Change"
XLU_actual$Sector = "Utilities"

#Energy ETF (XLE)
XLE_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLE_actual.xlsx', range = "A8:CP47")
XLE_actual <- as.data.frame(t(XLE_actual))
colnames(XLE_actual) <- XLE_actual[1,]
XLE_actual <- XLE_actual[-1,]
XLE_actual$date <- rownames(XLE_actual)
colnames(XLE_actual)[11] <- "EPS Change"
colnames(XLE_actual)[12] <- "Dividends Change"
XLE_actual$Sector = "Energy"

#Industrial ETF (XLI)
XLI_actual<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLI_actual.xlsx', range = "A8:CP47")
XLI_actual <- as.data.frame(t(XLI_actual))
colnames(XLI_actual) <- XLI_actual[1,]
XLI_actual <- XLI_actual[-1,]
XLI_actual$date <- rownames(XLI_actual)
colnames(XLI_actual)[11] <- "EPS Change"
colnames(XLI_actual)[12] <- "Dividends Change"
XLI_actual$Sector = "Industrial"

#Combining all the data
combined_data <- rbind(IYZ_actual,IYR_actual,XLB_actual,XLP_actual,XLY_actual,XLV_actual,XLK_actual,XLU_actual,XLE_actual,XLI_actual)

#CHECK 1: WHAT TO DO WITH DIFFERENT COLUMNS
combined_data <- rbind(combined_data,XLF_actual)
data <- combined_data

#Data Cleaning and Merging
data$month <- ifelse(substr(data$date,1,3) == "MAR",1,0)
data$month <- ifelse(substr(data$date,1,3) == "JUN",4,data$month)
data$month <- ifelse(substr(data$date,1,3) == "SEP",7,data$month)
data$month <- ifelse(substr(data$date,1,3) == "DEC",10,data$month)

data$year <- paste0("20",substr(data$date,6,7))

#Importing the macro data
library(lubridate)
macro<- read_excel('C:/Users/Sarmishtha Jain/Downloads/realfedfunds.xlsx')
macro$Year <- year(macro$DATE)
macro$Month <- month(macro$DATE)

#Formatting dates for merging
data$my <- paste0(data$year,data$month)
macro$my <- paste0(macro$Year,macro$Month)

save(data, file = "data.rda")
save(macro, file = "macro.rda")

#Merging the ETF data with the macro data and filtering from DEC 2001
final<-merge(data,macro, by.x="my", by.y = "my")
final <- final[(final$year ==2001 & final$month ==10) | final$year >2001,]

#Creating dummy variable for sectors
final$Industrials <- ifelse(final$Sector == "Industrial",1,0)
final$HealthCare <- ifelse(final$Sector == "Healthcare",1,0)
final$ConsumerDiscretionary <- ifelse(final$Sector == "Consumer Discretionary",1,0)
final$InformationTechnology <- ifelse(final$Sector == "Technology",1,0)
final$Energy <- ifelse(final$Sector == "Energy",1,0)
final$Financials <- ifelse(final$Sector == "Financials",1,0)
final$ConsumerStaples <- ifelse(final$Sector == "Consumer Staples",1,0)
final$Materials <- ifelse(final$Sector == "Materials",1,0)
final$RealEstate <- ifelse(final$Sector == "Real Estate",1,0)
final$Utilities <- ifelse(final$Sector == "Utilities",1,0)
final$CommunicationServices <- ifelse(final$Sector == "Telecommunication",1,0)
final$Industrials <- factor(final$Industrials)
final$HealthCare <- factor(final$HealthCare)
final$ConsumerDiscretionary <- factor(final$ConsumerDiscretionary)
final$InformationTechnology <- factor(final$InformationTechnology)

#Dropping columns not required for modeling purposes
drop_final <- c("Year","Month","my","year","month", "date","Growth - Year over Year %","Margins (%)","Ratios","Per Share","SG&A Growth")
combined_actuals <- final[,!(names(final) %in% drop_final)]

save(final, file = "unclean_actuals.rda")

#Removing invalid data/NAs
#str = "https://my.apps.factset.com"
#final_new <- c()
#for (i in 1:nrow(combined_actuals)){
#final_new <- append(final_new,ifelse(sum(combined_actuals[i,1:35] >= str) == 35,i,0))
#}
#combined_actuals <- combined_actuals[-final_new,]
#combined_actuals[] <- lapply(combined_actuals, function(x) replace(x, grep(str, x),0))

#Converting the data into numeric format
combined_actual <- sapply(combined_actuals[,1:34], function(x) as.numeric(x))
combined_actuals <- cbind(combined_actual,combined_actuals[,35:50])
combined_actuals <- as.data.frame(combined_actuals)

#final <- final %>% 
# mutate(x_lag1 = lag(final, 1), y_lag1 = lag(y, 1))

#Saving the factset data as an RDA file
save(combined_actuals, file = "factset_actuals.rda")



#Data Extraction and Cleaning of Forecasted Data from Factset

#Real Estate Index Fund
IYR_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/IYR_forecast.xlsx', range = "A3:P5")
IYR_forecast <- as.data.frame(t(IYR_forecast))
colnames(IYR_forecast) <- IYR_forecast[1,]
IYR_forecast <- IYR_forecast[-1,]
IYR_forecast$date <- rownames(IYR_forecast)
IYR_forecast$Sector = "Real Estate"

#Telecommunication ETF
IYZ_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/IYZ_forecast.xlsx', range = "A3:P5")
IYZ_forecast <- as.data.frame(t(IYZ_forecast))
colnames(IYZ_forecast) <- IYZ_forecast[1,]
IYZ_forecast <- IYZ_forecast[-1,]
IYZ_forecast$date <- rownames(IYZ_forecast)
IYZ_forecast$Sector = "Telecommunication"

#Materials ETF
XLB_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLB_forecast.xlsx', range = "A3:P5")
XLB_forecast <- as.data.frame(t(XLB_forecast))
colnames(XLB_forecast) <- XLB_forecast[1,]
XLB_forecast <- XLB_forecast[-1,]
XLB_forecast$date <- rownames(XLB_forecast)
XLB_forecast$Sector = "Materials"

#Consumer Staples ETF
XLP_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLP_forecast.xlsx', range = "A3:P5")
XLP_forecast <- as.data.frame(t(XLP_forecast))
colnames(XLP_forecast) <- XLP_forecast[1,]
XLP_forecast <- XLP_forecast[-1,]
XLP_forecast$date <- rownames(XLP_forecast)
XLP_forecast$Sector = "Consumer Staples"

#Consumer Discretionary ETF
XLY_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLY_forecast.xlsx', range = "A3:P5")
XLY_forecast <- as.data.frame(t(XLY_forecast))
colnames(XLY_forecast) <- XLY_forecast[1,]
XLY_forecast <- XLY_forecast[-1,]
XLY_forecast$date <- rownames(XLY_forecast)
XLY_forecast$Sector = "Consumer Discretionary"

#Healthcare ETF
XLV_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLV_forecast.xlsx', range = "A3:P5")
XLV_forecast <- as.data.frame(t(XLV_forecast))
colnames(XLV_forecast) <- XLV_forecast[1,]
XLV_forecast <- XLV_forecast[-1,]
XLV_forecast$date <- rownames(XLV_forecast)
XLV_forecast$Sector = "Healthcare"

#Technology ETF
XLK_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLK_forecast.xlsx', range = "A3:P5")
XLK_forecast <- as.data.frame(t(XLK_forecast))
colnames(XLK_forecast) <- XLK_forecast[1,]
XLK_forecast <- XLK_forecast[-1,]
XLK_forecast$date <- rownames(XLK_forecast)
XLK_forecast$Sector = "Technology"

#Financials ETF
XLF_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLF_forecast.xlsx', range = "A3:P5")
XLF_forecast <- as.data.frame(t(XLF_forecast))
colnames(XLF_forecast) <- XLF_forecast[1,]
XLF_forecast <- XLF_forecast[-1,]
XLF_forecast$date <- rownames(XLF_forecast)
XLF_forecast$Sector = "Financials"

#Utilities ETF
XLU_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLU_forecast.xlsx', range = "A3:P5")
XLU_forecast <- as.data.frame(t(XLU_forecast))
colnames(XLU_forecast) <- XLU_forecast[1,]
XLU_forecast <- XLU_forecast[-1,]
XLU_forecast$date <- rownames(XLU_forecast)
XLU_forecast$Sector = "Utilities"

#Energy ETF
XLE_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLE_forecast.xlsx', range = "A3:P5")
XLE_forecast <- as.data.frame(t(XLE_forecast))
colnames(XLE_forecast) <- XLE_forecast[1,]
XLE_forecast <- XLE_forecast[-1,]
XLE_forecast$date <- rownames(XLE_forecast)
XLE_forecast$Sector = "Energy"

#Industrial ETF
XLI_forecast<-read_excel('C:/Users/Sarmishtha Jain/Downloads/XLI_forecast.xlsx', range = "A3:P5")
XLI_forecast <- as.data.frame(t(XLI_forecast))
colnames(XLI_forecast) <- XLI_forecast[1,]
XLI_forecast <- XLI_forecast[-1,]
XLI_forecast$date <- rownames(XLI_forecast)
XLI_forecast$Sector = "Industrial"

#Combined forecasted data
combined_forecasts <- rbind(XLB_forecast,XLE_forecast,IYR_forecast,XLI_forecast,XLY_forecast,IYZ_forecast,XLU_forecast,XLK_forecast,XLF_forecast,XLP_forecast,XLV_forecast)
combined_forecasts$DATE <- paste0("20",substr(combined_forecasts$date,6,7),"-10-01")

#Saving the forecast factset data as an RDA file
save(combined_forecasts, file = "factset_forecasts.rda")
