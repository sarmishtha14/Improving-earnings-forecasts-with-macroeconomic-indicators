# Improving-earnings-forecasts-with-macroeconomic-indicators

##Contents of this file
 - About The Project
 - Getting Started
 - Usage
 - Contact
 - Acknowledgements

##About The Project
This project aims to improve earnings per share forecasts of 11 sector ETFs by using a variety of different macro-economic indicators such as federal funds rate, inflation rate, building permits, money stock, etc. We are exploring the effect of macro indicators on the EPS and comparing our model to existing models of other analysts and improving on them.

###Built With
This project is built with a variety of packages and softwares:
- RStudio
- RPostgres
- xts
- quantmod
- data.table
- tiyverse
- ggplot2
- dplyr
- MASS
- stats
- corrplot
- forecast

###Data
The data is found from multiple sources such as:
- Compustat
- IBES
- CRSP
- WRDS
- Factset

##Getting Started
Install RStudio on your desktop and then install the above mentioned packages with the command:
install.packages("package_name")
The package can then be loaded in the system through the library function which is in the code itself
library("package_name")

###Files
For this project a variety of files were used for different purposes:

1. Capstone_ibes+compustat.R
This file is used to extract the data from Compustat and IBES using Postgre database through WRDS system. It then performs cleaning of the data along with segregation into different sectors and limiting it to small cap firms.

2. Data Cleaning.R
This file reads the sector ETF data for all 11 sectors which were downloaded from Factset. This file then performs the cleaning of the data and combines all the 11 sectors into 1 dataframe. It does this for the actual ETF data from 2001 to 2023 and for the forecasted data of the analysts for the period 2020 to 2023.

3. VaR_macroplots.R
This file performs VaR analysis of different macroeconomic indicators on the different sectors. 

4. Capstone_quarterly_Materials&ConsumerStaples.R
This file performs EDA and time-series modeling of EPS on quarterly data of the ETFs for the sectors Materials and Consumer Staples.

5. Capstone_annual_Materials&ConsumerStaples.R
This file performs EDA and time-series modeling of EPS on annual data of the ETFS for the sectors Materials and Consumer Staples and macro indicators along with comparison with analysts forecasts.

6. Capstone_annual_Utilities&RealEstate&Telecom.R
This file performs EDA and time-series modeling of EPS on annual data of the ETFS for the sectors Utilities, Real Estate and Telecommuniction and macro indicators along with comparison with analysts forecasts.

7. Capstone_Healthcare and Consumer Discretionary.R
This file performs EDA and time-series modeling of EPS on annual data of the ETFS for the sectors Healthcare and Consumer Discretionary and macro indicators along with comparison with analysts forecasts.

8. Capstone_IT&Energy.R
This file performs EDA and time-series modeling of EPS on annual data with of the ETFS for the sectors Information Technology and Energy and macro indicators along with comparison with analysts forecasts.

9. Capstone_Industrials.R
This file performs EDA and time-series modeling of EPS on annual data with of the ETFS for the sectors Industrials and Financials and macro indicators along with comparison with analysts forecasts.
