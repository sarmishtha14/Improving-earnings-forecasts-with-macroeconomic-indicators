library(data.table)
library(tidyverse)
library(dplyr)
library(xts)
library(quantmod)
library(RPostgres)

source("C:/Users/Sarmishtha Jain/OneDrive/Desktop/Duke/Spring Term 2/Capstone/login_credentials.R")


#install.packages("RPostgres", repos = "https://r-dbi.r-universe.dev")

wrds<-dbConnect(Postgres(),  
                host='wrds-pgdata.wharton.upenn.edu',
                port=9737,
                user=username,	            # replace with your WRDS user name			
                password=password,            # replace with your WRDS password
                sslmode='require',
                dbname='wrds')

#Querying compustat data

#res <- dbSendQuery(wrds,"SELECT DISTINCT comp.secd.tic, comp.fundq.datadate, comp.fundq.fyearq, comp.fundq.fqtr, comp.fundq.niq, comp.fundq.epsfxq, comp.fundq.cshoq, comp.fundq.mkvaltq
#                        FROM comp.fundq
#                        INNER JOIN comp.secd ON comp.fundq.gvkey = comp.secd.gvkey
#                        WHERE comp.fundq.mkvaltq >= 250 AND comp.fundq.mkvaltq <= 2000")

res <- dbSendQuery(wrds,"SELECT DISTINCT C1.cusip,C1.gvkey,MAX(gicdesc) OVER (PARTITION BY C1.cusip) AS sector
                    FROM wrdsapps_finratio_ibes.firm_ratio_ibes AS C1
                    JOIN (SELECT cusip,MAX(mktcap) AS market_cap FROM wrdsapps_finratio.firm_ratio GROUP BY cusip) AS C2 ON C1.cusip = C2.cusip 
                    WHERE market_cap>=250 AND market_cap<=2000")


comp <- dbFetch(res, n=-1)
View(comp)


gvkey <- paste0("'",comp$gvkey,"'",collapse = ",")
ticker <- comp$gvkey
query <- paste0("SELECT DISTINCT gvkey,cusip,datadate,fyearq,ajexq,ajpq, niq, epsfxq, cshoq,cshtrq,prccq,prchq,prclq FROM comp.fundq WHERE comp.fundq.gvkey IN (",gvkey,") ORDER BY comp.fundq.datadate")
#query <- paste0("SELECT * FROM (SELECT *,MAX(statpers) OVER(PARTITION BY ticker,fpedats) AS max_statpers FROM tr_ibes.statsum_epsus WHERE cusip IN (",tic,") AND curcode = 'USD' AND fpi = '6') AS I WHERE statpers = max_statpers ORDER BY ticker,fpedats"))

# Change "comp.secd.tic" to your desired ticker. 
res_ <- dbSendQuery(wrds, query)
comp2 <- dbFetch(res_, n=-1)
View(comp2)

comp_final <- merge(comp2,comp,by = "gvkey")
View(comp_final)

#Saving the Compustat data as an RDA file
save(comp_final, file = "compustatdata.rda")

#Loading the Compustat data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/compustatdata.rda")

#IBES data

res1 <- dbSendQuery(wrds,"SELECT DISTINCT C1.cusip,C1.gvkey,MAX(gicdesc) OVER (PARTITION BY C1.cusip) AS sector
                    FROM wrdsapps_finratio_ibes.firm_ratio_ibes AS C1
                    JOIN (SELECT cusip,MAX(mktcap) AS market_cap FROM wrdsapps_finratio.firm_ratio GROUP BY cusip) AS C2 ON C1.cusip = C2.cusip 
                    WHERE market_cap>=250 AND market_cap<=2000")


ibes_tic <- dbFetch(res1, n=-1)
View(ibes_tic)

tic <- paste0("'",unique(ibes_tic$cusip),"'",collapse = ",")
ticker <- unique(ibes_tic$cusip)


query_ibes <- paste0("SELECT * FROM (SELECT *,MAX(statpers) OVER(PARTITION BY ticker,fpedats) AS max_statpers FROM tr_ibes.statsum_epsus WHERE cusip IN (",tic,") AND curcode = 'USD' AND fpi = '6') AS I WHERE statpers = max_statpers ORDER BY ticker,fpedats")
res2 <- dbSendQuery(wrds,query_ibes)


ibes <- dbFetch(res2, n=-1)
View(ibes)

ibes_final <- merge(ibes,ibes_tic,by = "cusip")
View(ibes_final)


#Cleaning the data Compustat

#Creating a variable for quarter
comp_final$qtr <- ifelse(month(comp_final$datadate) == 1 | month(comp_final$datadate) == 2 | month(comp_final$datadate) == 3,1,0)
comp_final$qtr <- ifelse(month(comp_final$datadate) == 4 | month(comp_final$datadate) == 5 | month(comp_final$datadate) == 6,2,comp_final$qtr)
comp_final$qtr <- ifelse(month(comp_final$datadate) == 7 | month(comp_final$datadate) == 8 | month(comp_final$datadate) == 9,3,comp_final$qtr)
comp_final$qtr <- ifelse(month(comp_final$datadate) == 10 | month(comp_final$datadate) == 11 | month(comp_final$datadate) == 12,4,comp_final$qtr)
comp_final$dte <- paste0(comp_final$fyearq,comp_final$qtr)

#Creating dummy variable for sector

comp_final$Industrials <- ifelse(comp_final$sector == "Industrials",1,0)
comp_final$Industrials <- ifelse(is.na(comp_final$sector),0,comp_final$Industrials)
comp_final$HealthCare <- ifelse(comp_final$sector == "Health Care",1,0)
comp_final$HealthCare <- ifelse(is.na(comp_final$sector),0,comp_final$HealthCare)
comp_final$ConsumerDiscretionary <- ifelse(comp_final$sector == "Consumer Discretionary",1,0)
comp_final$ConsumerDiscretionary <- ifelse(is.na(comp_final$sector),0,comp_final$ConsumerDiscretionary)
comp_final$InformationTechnology <- ifelse(comp_final$sector == "Information Technology",1,0)
comp_final$InformationTechnology <- ifelse(is.na(comp_final$sector),0,comp_final$InformationTechnology)
comp_final$Energy <- ifelse(comp_final$sector == "Energy",1,0)
comp_final$Energy <- ifelse(is.na(comp_final$sector),0,comp_final$Energy)
comp_final$Financials <- ifelse(comp_final$sector == "Financials",1,0)
comp_final$Financials <- ifelse(is.na(comp_final$sector),0,comp_final$Financials)
comp_final$ConsumerStaples <- ifelse(comp_final$sector == "Consumer Staples",1,0)
comp_final$ConsumerStaples <- ifelse(is.na(comp_final$sector),0,comp_final$ConsumerStaples)
comp_final$Materials <- ifelse(comp_final$sector == "Materials",1,0)
comp_final$Materials <- ifelse(is.na(comp_final$sector),0,comp_final$Materials)
comp_final$RealEstate <- ifelse(comp_final$sector == "Real Estate",1,0)
comp_final$RealEstate <- ifelse(is.na(comp_final$sector),0,comp_final$RealEstate)
comp_final$Utilities <- ifelse(comp_final$sector == "Utilities",1,0)
comp_final$Utilities <- ifelse(is.na(comp_final$sector),0,comp_final$Utilities)
comp_final$CommunicationServices <- ifelse(comp_final$sector == "Communication Services",1,0)
comp_final$CommunicationServices <- ifelse(is.na(comp_final$sector),0,comp_final$CommunicationServices)

#Creating dummy variable for quarter
comp_final$Qtr1 <- ifelse(comp_final$qtr == 1,1,0)
comp_final$Qtr2 <- ifelse(comp_final$qtr == 2,1,0)
comp_final$Qtr3 <- ifelse(comp_final$qtr == 3,1,0)
comp_final$Qtr4 <- ifelse(comp_final$qtr == 4,1,0)

library("readxl")
macros <-read_excel("C:/Users/Sarmishtha Jain/OneDrive/Desktop/Duke/Spring Term 2/Capstone/realfedfunds.xlsx")
macros <- as.data.frame(macros)
macros$qtr <- ifelse(month(macros$DATE) == 1,1,0)
macros$qtr <- ifelse(month(macros$DATE) == 4,2,macros$qtr)
macros$qtr <- ifelse(month(macros$DATE) == 7,3,macros$qtr)
macros$qtr <- ifelse(month(macros$DATE) == 10,4,macros$qtr)
macros$dte <- paste0(year(macros$DATE),macros$qtr)

macros_comp <- merge(comp_final,macros, by = "dte")
View(macros_comp)
drop_comp <- c("sector","cusip.x","cusip.y","fyearq","qtr.x","qtr.y","DATE","qtr","dte","depcy")

comp_clean <- macros_comp[,!(names(macros_comp) %in% drop_comp)]
View(comp_clean)
#comp_clean[is.na(comp_clean)]<- 0
comp_clean1 <- comp_clean[!is.na(comp_clean$prclq),]
comp_clean1 <- comp_clean1 %>% replace(is.na(.), 0)
timeseries_data <- ts(comp_clean1$epsfxq)
drop_clean <- c("gvkey","datadate")
comp_clean <- comp_clean1[,!(names(comp_clean1) %in% drop_clean)]

#Cleaning the data IBES

#Removing Outliers (Firms whose cusip and gvkeys dont match)
ibes_clean <- ibes_final[,1:27]
ibes_clean <- unique(ibes_clean)
ibes_clean <- merge(ibes_clean,ibes_final[,c(1,29)], by = "cusip")


#Saving the IBES data as an RDA file
save(ibes_final, file = "ibesdata.rda")

#Loading the IBES data
load("C:/Users/Sarmishtha Jain/OneDrive/Documents/ibesdata.rda")


#Getting the data with actual data from FactSet


#Modeling

#Linear Regression model
model1 <- lm(epsfxq ~ .,data = comp_clean)
summary(model1)

#Linear Regression model with interactions
model2 <- lm(epsfxq ~ .^2,data = comp_clean)
summary(model2)

#Time Series Analysis
install.packages("caTools")
library(caTools)
data_split = sample.split(comp_clean, SplitRatio = 0.75)
train <- subset(comp_clean, data_split == TRUE)
test <-subset(comp_clean, data_split == FALSE)
model3 <- lm(epsfxq ~ .,data = train)
summary(model3)
#comp_Train <- comp_clean[comp_clean$date < "2022-01-01",]
#comp_Test <- comp_clean[comp_clean$date >= '2022-01-01',]

#Moving average regression model
install.packages("smooth")
require(smooth)
model4 <- sma(comp_clean, h=18, silent=FALSE)
summary(model4)

#ARIMA model
log_timeseries <- round(ifelse(timeseries_data <= 0,0,log(timeseries_data)),4)
log_timeseries <- log_timeseries %>% replace(is.na(.), 0)
model5 <- arima(log_timeseries, c(0, 0, 1))
model5

#IBES actual standard error
ibes_final$
ibes_final$stderr <- abs(ibes_final$actual - comp_final$actual)


