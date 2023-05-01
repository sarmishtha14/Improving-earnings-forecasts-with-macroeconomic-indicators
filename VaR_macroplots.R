library(quantmod)
library(vars)
library(xts)
library(zoo)

# Get macroeconomic variables from FRED
getSymbols("GDP", src = "FRED")
getSymbols("CPALTT01USM657N", src = "FRED") # Inflation Rate
getSymbols("UNRATE", src = "FRED") # Unemployment Rate
getSymbols("FEDFUNDS", src = "FRED") # Federal Funds Rate
getSymbols("M2SL", src = "FRED") # M2 Money Stock
getSymbols("PERMIT", src = "FRED") # Building Permits

PMI <- read.csv("C:/Users/Sarmishtha Jain/Downloads/PMI_NMI DATA.csv", header = TRUE) #Purchasing Managers' Index (PMI)
PMI$Date <- as.Date(PMI$Date)
PMI_xts <- xts(PMI$Purchasing.Managers..Index..PMI., order.by = PMI$Date) #already quarterly


# Get IYZ price history from Yahoo Finance
getSymbols("IYZ", src = "yahoo", from = "2000-01-01")

# Calculate IYZ's quarterly log returns
IYZ_quarterly <- to.quarterly(IYZ[, "IYZ.Adjusted"])
IYZ_returns <- diff(log(IYZ_quarterly[, "IYZ[, \"IYZ.Adjusted\"].Close"]))

# Convert monthly data to quarterly data
quarterly_inflation <- aggregate(CPALTT01USM657N, as.yearqtr, mean)
quarterly_unemployment <- aggregate(UNRATE, as.yearqtr, mean)
quarterly_fedfunds <- aggregate(FEDFUNDS, as.yearqtr, mean)
quarterly_m2 <- aggregate(M2SL, as.yearqtr, mean)
quarterly_permits <- aggregate(PERMIT, as.yearqtr, mean)

# Calculate the GDP growth
GDP_growth <- diff(log(GDP)) * 100

# Define start and end as yearqtr objects
start_date <- as.yearqtr("2002 Q1")
end_date <- as.yearqtr("2022 Q4")

# Align all the xts objects to the desired date range
GDP_growth_aligned <- window(GDP_growth, start = start_date, end = end_date)
inflation_rate_aligned <- window(quarterly_inflation, start = start_date, end = end_date)
unemployment_rate_aligned <- window(quarterly_unemployment, start = start_date, end = end_date)
federal_funds_rate_aligned <- window(quarterly_fedfunds, start = start_date, end = end_date)
m2_aligned <- window(quarterly_m2, start = start_date, end = end_date)
permits_aligned <- window(quarterly_permits, start = start_date, end = end_date)
IYZ_returns_aligned <- window(IYZ_returns, start = start_date, end = end_date)
PMI_aligned <- window(PMI_xts, start = start_date, end = end_date)

# Combine all the xts objects into a single xts object
data_xts <- merge(GDP_growth_aligned, inflation_rate_aligned, unemployment_rate_aligned, federal_funds_rate_aligned, m2_aligned, permits_aligned, IYZ_returns_aligned, PMI_aligned)
colnames(data_xts) <- c("GDP_growth", "inflation_rate", "unemployment_rate", "federal_funds_rate", "m2", "permits", "IYZ_returns", "PMI")

# Convert xts objects to time series
GDP_growth_ts <- ts(data_xts$GDP_growth, start = c(2002, 1), end = c(2022, 4), frequency = 4)
inflation_rate_ts <- ts(data_xts$inflation_rate, start = c(2002, 1), end = c(2022, 4), frequency = 4)
unemployment_rate_ts <- ts(data_xts$unemployment_rate, start = c(2002, 1), end = c(2022, 4), frequency = 4)
federal_funds_rate_ts <- ts(data_xts$federal_funds_rate, start = c(2002, 1), end = c(2022, 4), frequency = 4)
m2_ts <- ts(data_xts$m2, start = c(2002, 1), end = c(2022, 4), frequency = 4)
building_permits_ts <- ts(PERMIT, start = c(2002, 1), end = c(2022, 4), frequency = 4) #absolute (not change)
IYZ_returns_ts <- ts(data_xts$IYZ_returns, start = c(2002, 1), end = c(2022, 4), frequency = 4)
PMI_ts <- ts(data_xts$PMI, start = c(2002, 1), end = c(2022, 4), frequency = 4)

# Combine all the time series into a single data.frame
data <- cbind(GDP_growth_ts, inflation_rate_ts, unemployment_rate_ts, federal_funds_rate_ts, m2_ts, building_permits_ts, IYZ_returns_ts, PMI_ts)
colnames(data) <- c("GDP_growth", "inflation_rate", "unemployment_rate", "federal_funds_rate", "m2", "building_permits", "IYZ_returns", "PMI")


# Specify a maximum lag order to consider
maxlag <- 4

# Compute AIC for lag orders 1 through maxlag
aic <- rep(NA, maxlag)
for (i in 1:maxlag) {
  fit <- VAR(data, p = i, type = "const", season = NULL)
  aic[i] <- AIC(fit)
}

# Plot AIC values against lag order
plot(1:maxlag, aic, type = "b", xlab = "Lag Order", ylab = "AIC")

# Choose the lag order with the lowest AIC
optlag <- which.min(aic)


# Run VAR analysis with lag order of 4
var_model <- VAR(data, p = optlag)

# Plot IRFs for each macroeconomic variable on IYZ returns
par(mfrow = c(2, 4))
for (i in 1:8) {
  if (colnames(data)[i] != "IYZ_returns") { # Skip IYZ returns
    irf <- irf(var_model, impulse = colnames(data)[i], response = "IYZ_returns", n.ahead = 6, ortho = TRUE, ci = 0.95, boot = TRUE, runs = 100, cumulative = FALSE)
    vol_irf <- unlist(irf[1]$irf[1])
    vol_irf_lower <- unlist(irf[2]$Lower[1])
    vol_irf_upper <- unlist(irf[3]$Upper[1])
    plot(0:6, vol_irf, type = "l", lwd = 2, ylab = "Impact on IYZ Returns", xlab = "", cex.lab = 1.5, ylim = 1.2 * c(min(vol_irf_lower), max(vol_irf_upper)), main = colnames(data)[i])
    par(new = TRUE)
    plot(0:6, vol_irf_lower, type = "l", lwd = 1, ylab = "", xlab = "", lty = 4, ylim = 1.2 * c(min(vol_irf_lower), max(vol_irf_upper)))
    par(new = TRUE)
    plot(0:6, vol_irf_upper, type = "l", lwd = 1, ylab = "", xlab = "", lty = 4, ylim = 1.2 * c(min(vol_irf_lower), max(vol_irf_upper)))
    abline(h = 0, lty = 2, lwd = 1.5)
  }
}

